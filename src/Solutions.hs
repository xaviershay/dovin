module Solutions where
import qualified Solutions.ChannelFireball
import qualified Solutions.Core19_9
import qualified Solutions.Dominaria5
import qualified Solutions.Example
import qualified Solutions.ExplorersOfIxalanContest
import qualified Solutions.GuildsOfRavnica1
import qualified Solutions.GuildsOfRavnica3
import qualified Solutions.GuildsOfRavnica8
import qualified Solutions.GuildsOfRavnica9
import qualified Solutions.GuildsOfRavnicaPre2
import qualified Solutions.RivalsOfIxalan7
import qualified Solutions.UltimateMasters

all = [
  ("ChannelFireball", Solutions.ChannelFireball.solution, Solutions.ChannelFireball.formatter),
  ("Core19_9", Solutions.Core19_9.solution, Solutions.Core19_9.formatter),
  ("Dominaria5", Solutions.Dominaria5.solution, Solutions.Dominaria5.formatter),
  ("Example", Solutions.Example.solution, Solutions.Example.formatter),
  ("ExplorersOfIxalanContest", Solutions.ExplorersOfIxalanContest.solution, Solutions.ExplorersOfIxalanContest.formatter),
  ("GuildsOfRavnica1", Solutions.GuildsOfRavnica1.solution, Solutions.GuildsOfRavnica1.formatter),
  ("GuildsOfRavnica3", Solutions.GuildsOfRavnica3.solution, Solutions.GuildsOfRavnica3.formatter),
  ("GuildsOfRavnica8", Solutions.GuildsOfRavnica8.solution, Solutions.GuildsOfRavnica8.formatter),
  ("GuildsOfRavnica9", Solutions.GuildsOfRavnica9.solution, Solutions.GuildsOfRavnica9.formatter),
  ("GuildsOfRavnicaPre2", Solutions.GuildsOfRavnicaPre2.solution, Solutions.GuildsOfRavnicaPre2.formatter),
  ("RivalsOfIxalan7", Solutions.RivalsOfIxalan7.solution, Solutions.RivalsOfIxalan7.formatter),
  ("UltimateMasters", Solutions.UltimateMasters.solution, Solutions.UltimateMasters.formatter)
  ]
