module RPG.Data.StoryImpl
    ( runStory
    , dopestory
    , mkCoStory
    ) where

import Data.Pairing
import Control.Monad (void)
import RPG.Data.Story
import Control.Monad.Free
import Control.Comonad.Cofree

-- runStory :: Story a -> IO a
-- runStory (Pure a) = return a
-- runStory (Free a) = runStoryF a


-- runStoryF :: StoryF (Story a) -> IO a
-- runStoryF (Change c ct next) = do
--     putStrLn . (++ ".") . concat $ case ct of
--       Kill who ->
--           [ show c, " kills ", show who ]
--       Want what ->
--           [ show c, " wants the ", show what ]
--       Learn (ChangeOf (ChangeResult c' _)) ->
--           [ show c, " learns about ", show c', "'s actions"]
--       Feel c' how ->
--           [ show c, " now considers ", show c', " a(n) ", show how ]
--       Die ->
--           [ show c, " dies" ]
--       Leave ->
--           [ show c, " leaves the scene" ]
--       _ ->
--           [ show c, " changed: ", show ct ]
--     runStory . next $ ChangeResult c ct

-- runStoryF (Macguffin next) = do
--     let thing = "Sandwich"
--     putStrLn $ "There is a thing everyone wants: a " ++ thing
--     runStory . next $ Desirable thing

-- runStoryF (Interrupt interrupted by next) = do
--     putStrLn "\nwhile..."
--     runStory interrupted
--     putStrLn "\nbut is interrupted by..."
--     b <- runStory by
--     putStrLn ""
--     runStory $ next b


mkCoStory :: CoStory Int
mkCoStory = coiter next start
  where
    next w = CoStoryF (coChange w) (coInterrupt w) (coMacguffin w)
    start = 0 :: Int

    coChange w c ct    = (ChangeResult c ct, w)
    -- TODO(sandy): use pair to extract the state, inject it into this. winning.
    coInterrupt w a a' = (undefined, w)
    coMacguffin w      = (Desirable $ show w, w + 1)

runStory :: CoStory a -> Story b -> b
runStory w m = pair (\_ b -> b) w m


dopestory :: Story Desirable
dopestory = do
    let johnny = Character "Mr. Monkey"
    let crab = Character "The Lord of Crabs"
    let scrub = Character "Jared"

    void . change johnny $ Feel scrub Friend
    thing <- macguffin
    want crab thing
    want scrub thing

    interrupt (void $ change crab Leave) $ do
        uh_oh <- kill crab scrub
        change johnny . Learn $ ChangeOf uh_oh
        change johnny $ Feel crab Enemy

    change crab Leave
    change johnny Leave
    kill johnny crab
    thing2 <- macguffin
    return thing2

