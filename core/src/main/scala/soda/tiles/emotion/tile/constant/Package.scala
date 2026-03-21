package soda.tiles.emotion.tile.constant

/*
 * This package contains classes to model the tiles.
 */

import   soda.tiles.emotion.entity.Context
import   soda.tiles.emotion.entity.Instance
import   soda.tiles.emotion.entity.TileMessage
import   soda.tiles.emotion.entity.TileMessageBuilder
import   soda.tiles.emotion.entity.TilePair
import   soda.tiles.emotion.entity.TileTriple





/*
directive lean
import Soda.tiles.emotion.entity.TileMessage
*/

/**
 * This tile returns a sorted sequence of resources, where each resource occurs exactly once.
 */

trait AllRuleTile
{



  def all_rules (message : TileMessage [Boolean] ) : Context =
    message .context

  def apply (message : TileMessage [Boolean] ) : TileMessage [Context] =
    TileMessageBuilder .mk .build (message .context) (message .instance) (
      all_rules (message)
    )

}

case class AllRuleTile_ () extends AllRuleTile

object AllRuleTile {
  def mk : AllRuleTile =
    AllRuleTile_ ()
}


/*
directive lean
import Soda.tiles.fairness.tool.TileMessage
*/

/**
 * This tile returns a sorted sequence of agents, where each agent occurs exactly once.
 */

trait AllStateTile
{



  def all_states (message : TileMessage [Boolean] ) : Instance =
    message .instance

  def apply (message : TileMessage [Boolean] ) : TileMessage [Instance] =
    TileMessageBuilder .mk .build (message .context) (message .instance) (
      all_states (message)
    )

}

case class AllStateTile_ () extends AllStateTile

object AllStateTile {
  def mk : AllStateTile =
    AllStateTile_ ()
}

