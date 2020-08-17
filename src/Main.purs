module Main where

import Prelude

import Data.Foldable (elem)
import Data.Maybe (Maybe(..), maybe)
import Data.String (Pattern(..), stripPrefix)
import Data.String as String
import Data.UUID (UUID, genUUID)
import Data.UUID as UUID
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Elmish (ComponentDef, DispatchMsgFn, ReactElement, Transition, fork, forks, handle, handleMaybe)
import Elmish.Boot (defaultMain)
import Elmish.Foreign (Foreign, readForeign)
import Elmish.HTML.Styled as H
import Elmish.React.DOM as R
import Feather as Feather
import Foreign.Object as F
import Pusher as Pusher
import Web.HTML as HTML
import Web.HTML.Location as Location
import Web.HTML.Window as Window

main :: Effect Unit
main = do
  defaultMain { def, elementId: "app" }

data State
  = Pub PublisherState
  | Sub SubscriberState

type PublisherState =
  { channel :: String
  }

type SubscriberState =
  { reaction :: Maybe String
  , reactionId :: Maybe UUID
  }

data Message
  = ClearReaction UUID
  | DelayClearReaction UUID
  | InitPubView String
  | SetChannel String
  | SetReaction String

def :: forall m. MonadAff m => ComponentDef m Message State
def = { init, update, view }
  where
    init :: Transition m Message State
    init = do
      forks \dispatch -> do
        hash <- liftEffect $ Location.hash =<< Window.location =<< HTML.window
        case stripPrefix (Pattern "#/") hash of
          Just channel ->
            liftEffect $ Pusher.subscribe channel (maybe (pure unit) (dispatch <<< SetReaction) <<< parseMessage)
          Nothing -> do
            channel <- liftEffect $ (String.take 8 <<< UUID.toString) <$> genUUID
            liftEffect $ dispatch $ InitPubView channel
      pure $ Sub
        { reaction: Nothing
        , reactionId: Nothing
        }

    update :: State -> Message -> Transition m Message State
    update s msg = case s, msg of
      Sub state, ClearReaction reactionId ->
        if state.reactionId == Just reactionId then
          pure $ Sub state { reaction = Nothing }
        else
          pure s
      Sub state, DelayClearReaction reactionId -> do
        fork do
          liftAff $ delay $ Milliseconds 5000.0
          pure $ ClearReaction reactionId
        pure $ Sub state { reactionId = Just reactionId }
      Sub state, InitPubView channel ->
        pure $ Pub { channel }
      Sub state, SetReaction reaction ->
        if reaction `elem` reactions then do
          fork do
            reactionId <- liftEffect genUUID
            pure $ DelayClearReaction reactionId
          pure $ Sub state { reaction = Just reaction }
        else
          pure s
      Sub _, _ ->
        pure s
      Pub state, SetChannel channel ->
        pure $ Pub state { channel = channel }
      Pub _, _ ->
        pure s

    view :: State -> DispatchMsgFn Message -> ReactElement
    view (Sub state) dispatch =
      H.div "absolute top-0 left-0" $
        case state.reaction of
          Just reaction ->
            H.div "text-6xl self-center" reaction
          Nothing ->
            R.empty
    view (Pub state) dispatch =
      H.div "container mx-auto mt-4"
        [ H.h2 "text-4xl" "React with…"
        , R.fragment $ reactionButton <$> reactions
        , H.h2 "text-4xl" "To channel…"
        , H.div "flex"
            [ H.input_ "border rounded-l px-3 py-2"
                { value: state.channel
                , onChange: handleMaybe dispatch (map SetChannel <<< eventTargetValue)
                }
            , H.a_ "bg-transparent hover:bg-blue-500 text-blue-700 font-semibold hover:text-white border hover:border-transparent rounded-r border-l-0 p-2"
                { target: "_blank"
                , href: "#/" <> state.channel
                } $
                Feather.externalLink { size: 18 }
            ]
        ]
      where
        reactionButton reaction =
          H.button_ "bg-transparent hover:bg-blue-500 text-blue-700 font-semibold hover:text-white py-2 px-3 border border-blue-500 hover:border-transparent rounded-full m-1 focus:outline-none"
            { onClick: handle dispatch $ SetReaction reaction
            }
            reaction

    reactions :: Array String
    reactions =
      [ "❤️"
      , "✨"
      , "😂"
      , "🔥"
      , "👍"
      , "😍"
      , "😊"
      , "👏"
      , "🤯"
      , "😬"
      ]

    parseMessage :: Foreign -> Maybe String
    parseMessage f = do
      obj <- readForeign f
      reaction <- F.lookup "reaction" obj
      readForeign reaction

    eventTargetValue :: Foreign -> Maybe String
    eventTargetValue =
      readForeign
        >=> F.lookup "target"
        >=> readForeign
        >=> F.lookup "value"
        >=> readForeign
