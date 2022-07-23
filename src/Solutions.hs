module Solutions where
import Control.Lens (view)
import Dovin.Types (stepNumber)

import qualified Solutions.ChannelFireball
import qualified Solutions.CommanderLegends
import qualified Solutions.Core19_9
import qualified Solutions.Dominaria5
import qualified Solutions.Example
import qualified Solutions.ExplorersOfIxalanContest
import qualified Solutions.ForgottenRealms1
import qualified Solutions.GuildsOfRavnica1
import qualified Solutions.GuildsOfRavnica3
import qualified Solutions.GuildsOfRavnica8
import qualified Solutions.GuildsOfRavnica9
import qualified Solutions.GuildsOfRavnicaPre2
import qualified Solutions.RavnicaAllegiance3
import qualified Solutions.RavnicaAllegiance4
import qualified Solutions.RavnicaAllegiance5
import qualified Solutions.RavnicaAllegiance7
import qualified Solutions.RavnicaAllegiancePre2
import qualified Solutions.RivalsOfIxalan7
import qualified Solutions.UltimateMasters
import qualified Solutions.WarOfTheSpark2

all = [
  ("ChannelFireball", Solutions.ChannelFireball.solution, Solutions.ChannelFireball.formatter),
  ("CommanderLegends", Solutions.CommanderLegends.solution, Solutions.CommanderLegends.formatter),
  ("Core19_9", Solutions.Core19_9.solution, Solutions.Core19_9.formatter . view stepNumber),
  ("Dominaria5", Solutions.Dominaria5.solution, Solutions.Dominaria5.formatter . view stepNumber),
  ("Example", Solutions.Example.solution, Solutions.Example.formatter),
  ("ExplorersOfIxalanContest", Solutions.ExplorersOfIxalanContest.solution, Solutions.ExplorersOfIxalanContest.formatter . view stepNumber),
  ("ForgottenRealms1", Solutions.ForgottenRealms1.solution, Solutions.ForgottenRealms1.formatter),
  ("GuildsOfRavnica1", Solutions.GuildsOfRavnica1.solution, Solutions.GuildsOfRavnica1.formatter . view stepNumber),
  ("GuildsOfRavnica3", Solutions.GuildsOfRavnica3.solution, Solutions.GuildsOfRavnica3.formatter . view stepNumber),
  ("GuildsOfRavnica8", Solutions.GuildsOfRavnica8.solution, Solutions.GuildsOfRavnica8.formatter),
  ("GuildsOfRavnica9", Solutions.GuildsOfRavnica9.solution, Solutions.GuildsOfRavnica9.formatter . view stepNumber),
  ("GuildsOfRavnicaPre2", Solutions.GuildsOfRavnicaPre2.solution, Solutions.GuildsOfRavnicaPre2.formatter . view stepNumber),
  ("RavnicaAllegiance3", Solutions.RavnicaAllegiance3.solution, Solutions.RavnicaAllegiance3.formatter),
  ("RavnicaAllegiance4", Solutions.RavnicaAllegiance4.solution, Solutions.RavnicaAllegiance4.formatter),
  ("RavnicaAllegiance5", Solutions.RavnicaAllegiance5.solution, Solutions.RavnicaAllegiance5.formatter),
  ("RavnicaAllegiance7", Solutions.RavnicaAllegiance7.solution, Solutions.RavnicaAllegiance7.formatter),
  ("RavnicaAllegiancePre2", Solutions.RavnicaAllegiancePre2.solution, Solutions.RavnicaAllegiancePre2.formatter),
  ("RivalsOfIxalan7", Solutions.RivalsOfIxalan7.solution, Solutions.RivalsOfIxalan7.formatter),
  ("UltimateMasters", Solutions.UltimateMasters.solution, Solutions.UltimateMasters.formatter),
  ("WarOfTheSpark2", Solutions.WarOfTheSpark2.solution, Solutions.WarOfTheSpark2.formatter)
  ]
