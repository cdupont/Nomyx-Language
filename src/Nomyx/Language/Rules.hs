{-# LANGUAGE ScopedTypeVariables #-}

-- | Basic rules building blocks.
-- for example, you can suppress rule 1 with:
--do
--   suppressRule 1

module Nomyx.Language.Rules (
   RuleNumber,
   RuleCode,
   RuleEvent(..),
   RuleStatus(..),
   MetaRule,
   activateRule, activateRule_,
   rejectRule, rejectRule_,
   getRules, getActiveRules, getRule,
   getRulesByNumbers,
   getRuleFuncs,
   addRule, addRule_, addRule',
   suppressRule, suppressRule_, suppressAllRules,
   proposeRule, modifyRule,
   autoActivate,
   activateOrRejectRule,
   simulate,
   metaruleVar, createMetaruleVar, addMetarule, testWithMetaRules, displayMetarules,
   legal, illegal, noPlayPlayer, immutableRule,
   autoDelete,
   eraseAllRules,
   getSelfRuleNumber, getSelfRule,
   onRuleProposed,
   showRule,
   ruleEvent
   ) where

import Prelude hiding (foldr)
import Nomyx.Language.Types
import Nomyx.Language.Outputs
import Nomyx.Language.Utils
import Imprevu
import Control.Lens
import Control.Monad
import Data.List
import Data.Maybe

-- * Rule management

-- | activate a rule: change its state to Active and execute it
activateRule :: RuleNumber -> Nomex Bool
activateRule = ActivateRule

activateRule_ :: RuleNumber -> Nomex ()
activateRule_ r = activateRule r >> return ()

-- | reject a rule: change its state to Suppressed and suppresses all its environment (events, variables, inputs, victory)
-- the rule can be activated again later
rejectRule :: RuleNumber -> Nomex Bool
rejectRule = RejectRule

rejectRule_ :: RuleNumber -> Nomex ()
rejectRule_ r = void $ rejectRule r

getRules :: Nomex [RuleInfo]
getRules = GetRules

getActiveRules :: Nomex [RuleInfo]
getActiveRules = filter ((== Active) . _rStatus) <$> getRules

getRule :: RuleNumber -> Nomex (Maybe RuleInfo)
getRule rn = do
   rs <- GetRules
   return $ find (\a -> a ^. rNumber == rn) rs

getRulesByNumbers :: [RuleNumber] -> Nomex [RuleInfo]
getRulesByNumbers = mapMaybeM getRule

getRuleFuncs :: Nomex [Rule]
getRuleFuncs = map _rRule <$> getRules

-- | add a rule to the game, it will have to be activated
addRule :: RuleInfo -> Nomex Bool
addRule = AddRule

addRule_ :: RuleInfo -> Nomex ()
addRule_ = void . AddRule

-- | add a rule to the game as described by the parameters
addRule' :: RuleName -> Rule -> RuleCode -> String -> Nomex RuleNumber
addRule' name rule code desc = do
   rns <- map _rNumber <$> getRules
   let number = head $ [1..] \\ rns
   res <- addRule $ defaultRuleInfo { _rRule = rule, _rNumber = number, _rRuleTemplate = defaultRuleTemplate {_rName = name,  _rRuleCode = code,  _rDescription = desc}}
   return $ if res then number else error "addRule': cannot add rule"


suppressRule :: RuleNumber -> Nomex Bool
suppressRule = RejectRule

suppressRule_ :: RuleNumber -> Nomex ()
suppressRule_ = void . RejectRule

suppressAllRules :: Nomex Bool
suppressAllRules = do
    rs <- getRules
    res <- mapM (suppressRule . _rNumber) rs
    return $ and res

modifyRule :: RuleNumber -> RuleInfo -> Nomex Bool
modifyRule = ModifyRule

-- | propose a rule that will need to be voted on.
proposeRule :: RuleInfo -> Nomex Bool
proposeRule = ProposeRule

-- | allows a rule to retrieve its own number (for auto-deleting for example)
getSelfRuleNumber :: Nomex RuleNumber
getSelfRuleNumber = SelfRuleNumber

getSelfRule :: Nomex RuleInfo
getSelfRule  = do
   srn <- getSelfRuleNumber
   [rs] <- getRulesByNumbers [srn]
   return rs

-- | activate or reject a rule
activateOrRejectRule :: RuleInfo -> Bool -> Nomex ()
activateOrRejectRule r b = if b then activateRule_ (_rNumber r) else rejectRule_ (_rNumber r)

-- | a rule can autodelete itself (generaly after having performed some actions)
autoDelete :: Nomex ()
autoDelete = getSelfRuleNumber >>= suppressRule_

-- | All rules from player p are erased:
eraseAllRules :: PlayerNumber -> Nomex Bool
eraseAllRules p = do
    rs <- getRules
    let myrs = filter (\a -> a ^. rProposedBy == p) rs
    res <- mapM (suppressRule . _rNumber) myrs
    return $ and res

-- | This rule will activate automatically any new rule.
autoActivate :: Nomex ()
autoActivate = void $ onEvent_ (ruleEvent Proposed) (activateRule_ . _rNumber)

-- * Meta Rules

-- | A meta rule is a rule that can juge the legality of another rule.
type MetaRule = RuleInfo -> Nomex Bool

-- | The meta rules are stored in a list variable
metaruleVar :: V [(String, MetaRule)]
metaruleVar = V "metarules"

-- | create the meta rule variable
createMetaruleVar :: Nomex ()
createMetaruleVar = void $ newVar' metaruleVar []

-- | add a new metarule to the list
addMetarule :: MetaRule -> String -> Nomex ()
addMetarule mr code = void $ modifyVar metaruleVar ((code, mr):)

-- | use the list of meta rules to juge a new rule
testWithMetaRules :: RuleInfo -> Nomex Bool
testWithMetaRules r = do
   mmrs <- readVar metaruleVar
   case mmrs of
      Just mrs -> and <$> mapM (($r) . snd) mrs
      Nothing ->  return False

displayMetarules :: Nomex ()
displayMetarules = void $ displayVar Nothing metaruleVar dispAll where
   dispAll mvs = return $ maybe "No meta rules" (("Meta Rules:\n" ++) . concatMap disp) mvs
   disp (s, _) = s ++ "\n"

-- | A rule will be always legal
legal :: MetaRule
legal = const $ return True

-- | A rule will be always illegal
illegal :: MetaRule
illegal = const $ return False


-- | Player p cannot propose any more rules
noPlayPlayer :: PlayerNumber -> MetaRule
noPlayPlayer pn rule = return $ _rProposedBy rule /= pn

-- | rule number rn cannot be deleted by any incoming rule
-- we simulate the execution of an incoming rule to make sure it doesn't delete the immutable rule
immutableRule :: RuleNumber -> MetaRule
immutableRule rn rule = do
   immu <- getRule rn
   maybe (return True) (const $ simulate (_rRule rule) (isJust <$> getRule rn)) immu

-- | simulate the execution of rule "sim" and then run rule "test" over the result
simulate :: Nomex a -> Nomex Bool -> Nomex Bool
simulate = Simu

-- | sets a callback called for each rule proposed
onRuleProposed :: (RuleInfo -> Nomex ()) -> Nomex ()
onRuleProposed f = void $ onEvent_ (ruleEvent Proposed) f

-- | a default rule
defaultRuleInfo :: RuleInfo
defaultRuleInfo = RuleInfo  {
    _rNumber       = 1,
    _rProposedBy   = 0,
    _rRule         = return (),
    _rStatus       = Pending,
    _rAssessedBy   = Nothing,
    _rModules      = [],
    _rRuleTemplate = defaultRuleTemplate}

defaultRuleTemplate :: RuleTemplate
defaultRuleTemplate = RuleTemplate {
    _rName         = "",
    _rDescription  = "",
    _rRuleCode     = "",
    _rAuthor       = "",
    _rPicture      = Nothing,
    _rCategory     = [],
    _rDeclarations = []}

mapMaybeM :: (Monad m) => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM f = liftM catMaybes . mapM f

showRule :: Show a => a -> Nomex ()
showRule x = void $ NewOutput Nothing (return $ show x)

-- | Build a event firing when an action is made on a rule
ruleEvent :: RuleEvent -> Event RuleInfo
ruleEvent re = SignalEvent $ Signal re
