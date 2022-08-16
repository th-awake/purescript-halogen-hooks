module Halogen.Hooks.Internal.Eval where

import Prelude

import Control.Applicative.Free (hoistFreeAp, liftFreeAp, retractFreeAp)
import Control.Monad.Free (Free, liftF, substFree)
import Data.Array as Array
import Data.Bifunctor (bimap)
import Data.Coyoneda (unCoyoneda)
import Data.Foldable (sequence_)
import Data.Maybe (Maybe(..), fromJust, fromMaybe, maybe)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Effect.Exception.Unsafe (unsafeThrow)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Foreign.Object as Object
import Halogen as H
import Halogen.Hooks.HookM (HookAp(..), HookF(..), HookM(..))
import Halogen.Hooks.Internal.Eval.Types (HalogenM', HookState(..), InterpretHookReason(..), InternalHookState, fromQueryFn, toQueryFn)
import Halogen.Hooks.Internal.Types (MemoValuesImpl, OutputValue, SlotType, fromMemoValue, fromMemoValues, toQueryValue)
import Halogen.Hooks.Internal.UseHookF (UseHookF(..))
import Halogen.Hooks.Types (StateId(..))
import Halogen.Query.HalogenM (HalogenAp(..))
import Partial.Unsafe (unsafePartial)
import Unsafe.Reference (unsafeRefEq)

mkEval
  :: forall q i m b a
   . (i -> i -> Boolean)
  -> (HalogenM' q i m b b -> HookM m ~> HalogenM' q i m b)
  -> (InterpretHookReason -> HalogenM' q i m b b)
  -> H.HalogenQ q (HookM m Unit) i a
  -> HalogenM' q i m b a
mkEval inputEq _evalHookM _evalHook = case _ of
  H.Initialize a -> do
    HookState { stateRef } <- H.get
    _ <- executeHooksAndEffects stateRef Initialize
    pure a

  H.Query q reply -> do
    HookState { stateRef } <- H.get
    let { queryFn } = unsafePerformEffect $ Ref.read stateRef
    case queryFn of
      Nothing ->
        pure (reply unit)
      Just fn ->
        _evalHookM (executeHooksAndEffects stateRef Step)
          $ unCoyoneda (\g -> map (maybe (reply unit) g) <<< (fromQueryFn fn)) q

  H.Action act a -> do
    HookState { stateRef } <- H.get
    _evalHookM (executeHooksAndEffects stateRef Step) act
    pure a

  H.Receive nextInput a -> do
    HookState { stateRef } <- H.get
    let { input: prevInput } = unsafePerformEffect $ Ref.read stateRef

    unless (inputEq prevInput nextInput) do
      let
        execute = unsafePerformEffect do
          _ <- Ref.modify_ (_ { input = nextInput }) stateRef
          pure (executeHooksAndEffects stateRef Step)

      void execute

    pure a

  H.Finalize a -> do
    HookState { stateRef } <- H.get
    _ <- executeHooksAndEffects stateRef Finalize
    pure a

  where
  executeHooksAndEffects
    :: Ref (InternalHookState q i m b)
    -> InterpretHookReason
    -> HalogenM' q i m b b
  executeHooksAndEffects stateRef reason = do
    _ <- _evalHook reason

    let { evalQueue } = unsafePerformEffect $ Ref.read stateRef

    when (not (Array.null evalQueue)) do
      let
        runSequence = unsafePerformEffect do
          Ref.modify_ (_ { evalQueue = [], stateDirty = false }) stateRef
          pure (sequence_ evalQueue)

      runSequence

      let
        { stateDirty } = unsafePerformEffect $ Ref.read stateRef
        initializeOrStepReason = reason == Initialize || reason == Step

      when (stateDirty && initializeOrStepReason) do
        void $ executeHooksAndEffects stateRef Step

    H.gets (_.result <<< unwrap)

evalHook
  :: forall q i m a
   . (HalogenM' q i m a a -> HookM m ~> HalogenM' q i m a)
  -> (InterpretHookReason -> HalogenM' q i m a a)
  -> InterpretHookReason
  -> Ref (InternalHookState q i m a)
  -> UseHookF m
       ~> Free (H.HalogenF (HookState q i m a) (HookM m Unit) SlotType OutputValue m)
evalHook _evalHookM _evalHook reason stateRef = case _ of
  UseState initial reply ->
    case reason of
      Initialize ->
        let
          identifier = unsafePerformEffect do
            { componentRef, stateCells } <- Ref.modify (\state -> state { stateCells { queue = Array.snoc state.stateCells.queue initial }}) stateRef
            pure (StateId (Tuple componentRef (Array.length stateCells.queue - 1)))

        in
          pure (reply (Tuple initial identifier))

      _ ->
        let
          { value, identifier } = unsafePerformEffect do
            { componentRef, stateCells: { index, queue } } <- Ref.read stateRef
            Ref.modify_ (_ { stateCells { index = stepIndex index queue } }) stateRef
            pure { value: unsafeGetCell index queue, identifier: StateId (Tuple componentRef index) }

        in
          pure (reply (Tuple value identifier))

  UseQuery _ handler a ->
    let
      handler' :: forall b. q b -> HookM m (Maybe b)
      handler' = handler <<< toQueryValue

    in pure $ unsafePerformEffect do
      _ <- Ref.modify_ (_ { queryFn = Just $ toQueryFn handler' }) stateRef
      pure a

  UseEffect mbMemos act a ->
    case reason of
      Initialize ->
        let
          eval = do
            mbFinalizer <- _evalHookM (_evalHook Queued) act

            let
              finalizer = fromMaybe (pure unit) mbFinalizer
              newQueue st = Array.snoc st.effectCells.queue (mbMemos /\ finalizer)

            pure $ unsafePerformEffect do
              _ <- Ref.modify_ (\s -> s { effectCells { queue = newQueue s } }) stateRef
              pure unit

        in
          pure $ unsafePerformEffect do
            Ref.modify_ (\s -> s { evalQueue = Array.snoc s.evalQueue eval }) stateRef
            pure a

      Queued ->
        pure a

      Step ->
        let
          { effectCells: { index, queue } } = unsafePerformEffect $ Ref.read stateRef
          nextIndex = stepIndex index queue
          mbOldMemos /\ finalizer = unsafeGetCell index queue

        in
          case mbMemos, mbOldMemos of
            Just newMemos, Just oldMemos ->
              let
                memos' :: { old :: MemoValuesImpl, new :: MemoValuesImpl }
                memos' = { old: fromMemoValues oldMemos, new: fromMemoValues newMemos }

              in
                if (Object.isEmpty memos'.new.memos || not memos'.new.eq memos'.old.memos memos'.new.memos) then
                  let
                    eval = do
                      mbFinalizer <- _evalHookM (_evalHook Queued) (finalizer *> act)

                      let
                        { effectCells: { queue: queue' } } = unsafePerformEffect $ Ref.read stateRef
                        newFinalizer = fromMaybe (pure unit) mbFinalizer
                        newValue = mbMemos /\ newFinalizer

                      pure $ unsafePerformEffect do
                        Ref.modify_ (_ { effectCells { queue = unsafeSetCell index newValue queue' } }) stateRef
                        pure unit

                  in
                    pure $ unsafePerformEffect do
                      Ref.modify_ (\s -> s
                        { evalQueue = Array.snoc s.evalQueue eval
                        , effectCells { index = nextIndex }
                        }) stateRef
                      pure a

                else
                  pure $ unsafePerformEffect do
                    Ref.modify_ (_ { effectCells { index = nextIndex } }) stateRef
                    pure a

            _, _ ->
              pure $ unsafePerformEffect do
                Ref.modify_ (_ { effectCells { index = nextIndex } }) stateRef
                pure a

      Finalize ->
        let
          { effectCells: { index, queue } } = unsafePerformEffect $ Ref.read stateRef
          _ /\ finalizer = unsafeGetCell index queue
          finalizeHook = _evalHookM (_evalHook Queued) finalizer

        in
          pure $ unsafePerformEffect do
            Ref.modify_ (\s -> s
              { evalQueue = Array.snoc s.evalQueue finalizeHook
              , effectCells { index = stepIndex index queue }
              }) stateRef
            pure a

  UseMemo memos memoFn reply ->
    case reason of
      Initialize ->
        let
          newValue = unsafePerformEffect do
            let memo = memoFn unit
            Ref.modify_ (\state -> state { memoCells { queue = Array.snoc state.memoCells.queue (memos /\ newValue) } }) stateRef
            pure memo

        in
          pure (reply newValue)

      _ ->
        let
          { memoCells: { index, queue } } = unsafePerformEffect $ Ref.read stateRef

          m =
            let
              oldMemos /\ oldValue = bimap fromMemoValues fromMemoValue (unsafeGetCell index queue)
              newMemos = fromMemoValues memos

            in
              { eq: newMemos.eq, old: oldMemos.memos, new: newMemos.memos, value: oldValue }

          nextIndex = stepIndex index queue

        in
          if (Object.isEmpty m.new || not (m.new `m.eq` m.old)) then
            let
              newValue = unsafePerformEffect do
                let memo = memoFn unit
                let newQueue = unsafeSetCell index (memos /\ memo) queue
                Ref.modify_ (_ { memoCells = { index: nextIndex, queue: newQueue } }) stateRef
                pure memo

            in
              pure (reply newValue)

          else
            pure $ unsafePerformEffect do
              Ref.modify_ (_ { memoCells { index = nextIndex } }) stateRef
              pure (reply m.value)

  UseRef initial reply ->
    case reason of
      Initialize ->
        let
          ref = unsafePerformEffect do
            newRef <- Ref.new initial
            Ref.modify_ (\state -> state { refCells { queue = Array.snoc state.refCells.queue newRef } }) stateRef
            pure newRef

        in
          pure (reply (Tuple initial ref))

      _ ->
        let
          { ref, value } = unsafePerformEffect do
            { refCells: { index, queue } } <- Ref.read stateRef
            Ref.modify_ (_ { refCells { index = stepIndex index queue } }) stateRef
            let ref = unsafeGetCell index queue
            value <- Ref.read ref
            pure { ref, value }

        in
          pure (reply (Tuple value ref))

evalHookM :: forall q i m a. HalogenM' q i m a a -> HookM m ~> HalogenM' q i m a
evalHookM (H.HalogenM runHooks) (HookM evalUseHookF) =
  H.HalogenM $ substFree interpretHalogenHook evalUseHookF
  where
  interpretHalogenHook :: HookF m ~> Free (H.HalogenF (HookState q i m a) (HookM m Unit) SlotType OutputValue m)
  interpretHalogenHook = case _ of
    Modify (StateId (Tuple ref id)) f reply -> do
      HookState { stateRef } <- liftF $ H.State \state -> Tuple state state

      let
        { componentRef, stateCells } = unsafePerformEffect $ Ref.read stateRef

      -- It is not safe to use `HookM` code which modifies state outside of the
      -- component that defines it, because the state identifiers are referring
      -- to an environment that potentially doesn't exist in the target component.
      --
      -- This leads either to unexpected state modifications or a crash when an
      -- index in state is accessed that doesn't exist.
      --
      -- NOTE: Using `unless` here throws an exception -- strictness? Using a
      -- case statement behaves as expected.
      case unsafeRefEq componentRef ref of
        true ->
          pure unit
        _ ->
          unsafeThrow "Attempted to use state-modifying `HookM` code outside the component where it was defined."

      let
        current = unsafeGetCell id stateCells.queue
        next = f current

      -- Like Halogen's implementation, `Modify` covers both get and set calls
      -- to `state`. We use the same `unsafeRefEq` technique to as Halogen does
      -- to ensure calls to `get` don't trigger evaluations / renders.
      case unsafeRefEq current next of
        true ->
          pure unit
        _ -> do
          let
            runHooks' = unsafePerformEffect do
              Ref.modify_ (\s -> s
                { stateCells { queue = unsafeSetCell id next s.stateCells.queue }
                , stateDirty = true
                }) stateRef
              pure (void runHooks)

          runHooks'

      pure (reply next)

    Subscribe emitter reply ->
      liftF $ H.Subscribe emitter reply

    Unsubscribe sid a ->
      liftF $ H.Unsubscribe sid a

    Lift f ->
      liftF $ H.Lift f

    ChildQuery box ->
      liftF $ H.ChildQuery box

    Raise o a ->
      liftF $ H.Raise o a

    Par (HookAp p) ->
      liftF $ H.Par $ retractFreeAp $ hoistFreeAp (HalogenAp <<< liftFreeAp <<< evalHookM (H.HalogenM runHooks)) p

    Fork hmu reply ->
      liftF $ H.Fork (evalHookM (H.HalogenM runHooks) hmu) reply

    Kill fid a ->
      liftF $ H.Kill fid a

    GetRef p reply ->
      liftF $ H.GetRef p reply

-- Read a cell for a hook
unsafeGetCell :: forall a. Int -> Array a -> a
unsafeGetCell index array = unsafePartial (Array.unsafeIndex array index)

-- Write a cell for a hook
unsafeSetCell :: forall a. Int -> a -> Array a -> Array a
unsafeSetCell index a array = unsafePartial (fromJust (Array.modifyAt index (const a) array))

stepIndex :: forall a. Int -> Array a -> Int
stepIndex index array = if index + 1 < Array.length array then index + 1 else 0

-- get :: forall q i m a. Ref (InternalHookState q i m a) -> InternalHookState q i m a
-- get = unsafePerformEffect <<< Ref.read
