module Dovin.Scratch where

type Card = String
type PlayerId = Int

data State = State
  { _statePhase :: PhaseStep
  , _statePriority :: PlayerId
  , _stateStack :: [Card]
  , _stateLog :: [Choice]
  , _stateChoices :: M.HashMap PlayerId [Choice]
  }

data R505_1_MainType = R505_1_PreCombat | R505_1_PostCombat

-- Shock: 2B: No choice needed. 2C Requires target creature or player.
--
data R601_CastingSteps =
  R601_2A_ProposeSpell
  | R601_2B_Configure
  | R601_2C_Target
  | R601_2D_AssignDamage
  | R601_2E_Legality
  | R601_2F_Cost
  | R601_2G_ManaAbilities
  | R601_2H_Pay
  | R601_2I_Trigger

data PhaseStep
  = R502_Untap
  | R503_Upkeep
  | R504_DrawStep -- See R120
  | R505_Main R505_1_MainType
  deriving (Show, Eq)

data R116_1_PriorityAction =
    R116_1A_Cast
  | R116_1B_Activate
  | R116_1C_Special
  | R116_1D_Mana
  deriving (Show, Eq)

data Choice = ChoosePriority (Maybe R116_1_PriorityAction)
  deriving (Show, Eq)

apply :: Player -> Choice -> State -> State
apply pid choice state =
  case view statePhase state of
    R505_FirstMain

  -- | BeginCombat
  -- | DeclareAttackers
  -- | DeclareBlockers
  -- | FirstStrikeDamage
  -- | CombatDamage
  -- | EndCombat
  -- | SecondMain
  -- | EndStep
