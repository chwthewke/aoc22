package aoc22

import org.scalacheck.Gen
import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class Aoc2Spec extends AnyWordSpec with Matchers with ScalaCheckDrivenPropertyChecks with TypeCheckedTripleEquals {

  import Aoc2.Move
  import Aoc2.Outcome

  "goal of duel" should {
    "be identity" in {
      forAll( Gen.oneOf( Move.values ), Gen.oneOf( Move.values ) ) { ( their, our ) =>
        Move.goal( their, Move.duel( their, our ) ) must ===( our )
      }
    }
  }

  "duel of goal" should {
    "be identity" in {
      forAll( Gen.oneOf( Move.values ), Gen.oneOf( Outcome.values ) ) { ( their, outcome ) =>
        Move.duel( their, Move.goal( their, outcome ) ) must ===( outcome )

      }
    }
  }

}
