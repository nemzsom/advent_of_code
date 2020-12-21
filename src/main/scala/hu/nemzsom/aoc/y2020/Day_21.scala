package hu.nemzsom.aoc.y2020

import hu.nemzsom.aoc.Solver

import scala.annotation.tailrec

object Day_21 extends App with Solver {

  override def solve(input: List[String]) = {
    val foods = FoodList.fromSpec(input)
    val allPossibleAllergens = foods.allPossibleAllergens
    foods.allIngredients.count(ingredient => !allPossibleAllergens.contains(ingredient)).toString
  }

  override def solveSecondPart(input: List[String]) = {
    val foods = FoodList.fromSpec(input)
    foods.resolveAllergens().sorted.map(_.food).mkString(",")
  }

  case class Food(ingredients: List[String], allergens: List[String]) {
    def possibleAllergens: Map[String, Set[String]] = allergens.map(al => (al, ingredients.toSet)).toMap
  }

  object Food {
    def apply(spec: String): Food = {
      val Array(ingredientsSpec, allergensSpec) = spec.split("\\(contains")
      val ingredients = ingredientsSpec.split(' ').filter(!_.isBlank).map(_.trim).toList
      val allergens = allergensSpec.filter(_ != ')').split(',').filter(!_.isBlank).map(_.trim).toList
      new Food(ingredients, allergens)
    }
  }

  case class FoodList(list: List[Food]) {
    def allergenMap: Map[String, Set[String]] =
      list.map(_.possibleAllergens).foldLeft(Map[String, Set[String]]()){ case (combined, foodAllergens) =>
        foodAllergens.foldLeft(combined) { case (map, (allergen, possibilities)) =>
          map.updated(allergen, map.getOrElse(allergen, Set()) ++ possibilities)
        }
      }

    def possibleAllergens: Map[String, Set[String]] =
      list.map(_.possibleAllergens).foldLeft(Map[String, Set[String]]()){ case (combined, foodAllergens) =>
        foodAllergens.foldLeft(combined) { case (map, (allergen, possibilities)) =>
          map.updated(allergen, map.getOrElse(allergen, possibilities).intersect(possibilities))
        }
      }

    def allPossibleAllergens: Set[String] =
      possibleAllergens.values.reduce(_ ++ _)

    def allIngredients: List[String] = list.flatMap(_.ingredients)

    @tailrec
    final def resolveAllergens(resolved: List[Allergen] = List(), possibles: Map[String, Set[String]] = possibleAllergens): List[Allergen] =
    if (possibles.isEmpty) resolved
    else {
      val allergen = possibles.find {
        case (_, foods) => foods.size == 1
      }.map { case (allergen, certain) => Allergen(certain.head, allergen) }.get
      val updatedPossibles = possibles.view.mapValues(_ - allergen.food).toMap.removed(allergen.allergen)
      resolveAllergens(allergen :: resolved, updatedPossibles)
    }
  }

  case class Allergen(food: String, allergen: String) extends Ordered[Allergen] {
    override def compare(that: Allergen): Int = allergen.compare(that.allergen)
  }

  object FoodList {
    def fromSpec(list: List[String]): FoodList = new FoodList(list.map(Food(_)))
  }

  solve()

  /*

    mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
    trh fvjkl sbzzf mxmxvkd (contains dairy)
    sqjhc fvjkl (contains soy)
    sqjhc mxmxvkd sbzzf (contains fish)


          allergenMap                                possible allergens

    dairy - Set(mxmxvkd kfcds sqjhc nhms)
            Set(trh fvjkl sbzzf mxmxvkd)      dairy intersect - Set(mxmxvkd)
    soy   - Set(sqjhc fvjkl)                  soy intersect   - Set(sqjhc, fvjkl)
    fish  - Set(mxmxvkd kfcds sqjhc nhms)
            Set(sqjhc mxmxvkd sbzzf)          fish intersect  - Set(mxmxvkd, sqjhc)


     All ingredients                                            all possible allergens (union)
     Set(mxmxvkd, kfcds, sqjhc, nhms, trh                       Set(mxmxvkd, sqjhc, fvjkl)
         fvjkl, sbzzf)


     All - (minus) possible allergens
     Set(kfcds, nhms, trh, sbzzf)

  */
}
