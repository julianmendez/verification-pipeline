package soda.tiles.emotion.tile.constant

/*
 * This package contains classes to model the tiles.
 */

import   soda.tiles.emotion.entity.Context
import   soda.tiles.emotion.entity.Instance
import   soda.tiles.emotion.entity.TileMessage
import   soda.tiles.emotion.entity.TileMessageBuilder





/*
directive lean
import Soda.tiles.emotion.entity.TileMessage
*/

/**
 * This tile returns the rules.
 */

trait RulesTile
{



  def get_rules (message : TileMessage [Boolean] ) : Context =
    message .context

  def apply (message : TileMessage [Boolean] ) : TileMessage [Context] =
    TileMessageBuilder .mk .build (message .context) (message .instance) (
      get_rules (message)
    )

}

case class RulesTile_ () extends RulesTile

object RulesTile {
  def mk : RulesTile =
    RulesTile_ ()
}


/*
directive lean
import Soda.tiles.fairness.tool.TileMessage
*/

/**
 * This tile returns the trajectory.
 */

trait TrajectoryTile
{



  def get_trajectory (message : TileMessage [Boolean] ) : Instance =
    message .instance

  def apply (message : TileMessage [Boolean] ) : TileMessage [Instance] =
    TileMessageBuilder .mk .build (message .context) (message .instance) (
      get_trajectory (message)
    )

}

case class TrajectoryTile_ () extends TrajectoryTile

object TrajectoryTile {
  def mk : TrajectoryTile =
    TrajectoryTile_ ()
}

