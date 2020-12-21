package hu.nemzsom.aoc.y2020

import hu.nemzsom.aoc.Solver

import scala.annotation.tailrec
import scala.util.matching.Regex

object Day_20 extends App with Solver {

  override def solve(input: List[String]) =
    AllMatches(parse(input)).corners.keys.product.toString

  override def solveSecondPart(input: List[String]) = {
    val image = AllMatches(parse(input)).reconstruct
    val result = image.removeBorders().join()
    val allHashCount = result.countHashes()
    val monsterCount = result.allTransformation.toList.map(SeaMonster.countMonsters).sum
    (allHashCount - (monsterCount * SeaMonster.monsterShape.size)).toString
  }

  case class Tile(id: Long, rows: Vector[PixelLine]) {

    def toImageString: String = {
      val tileStr = rows.map(line => line.toImagePixels).mkString(System.lineSeparator())
      s"Tile $id:${System.lineSeparator()}$tileStr"
    }

    def rotate90: Tile = Tile(id, rows.head.pixels.indices.map(column).toVector.reverse)
    def rotate180: Tile = rotate90.rotate90
    def rotate270: Tile = rotate180.rotate90
    def flip: Tile = Tile(id, rows.map(_.reverse))
    def allTransformation: Set[Tile] = Set(this, flip).flatMap(t => Set(t, t.rotate90, t.rotate180, t.rotate270))

    def column(i: Int): PixelLine = PixelLine(rows.map(_.pixelAt(i)))
    def pixelAt(pos: Pos): Char = rows(pos.y).pixelAt(pos.x)
    def isInside(pos: Pos): Boolean = pos.y >= 0 && pos.y < rows.length && pos.x >= 0 && pos.x < width

    def width: Int = rows.head.pixels.size

    def border(placement: Placement): PixelLine = placement match {
      case Top => rows.head
      case Bottom => rows.last
      case Left => column(0)
      case Right => column(width - 1)
    }

    def calculateMatches(t2: Tile): Set[Match] = {
      for {
        t2Transformed <- t2.allTransformation
        placement <- Set(Top, Bottom, Left, Right)
        if placement.matches(this, t2Transformed)
      } yield {
        Match(this, placement, t2Transformed)
      }
    }

    def removeBorders(): Tile = Tile(id, rows.tail.take(rows.size - 2).map(_.removeBorders()))

    def join(tile: Tile): Tile = Tile(id + tile.id, rows.zip(tile.rows).map { case (line1, line2) => line1.join(line2)})

    def countHashes(): Int = rows.map(_.countHashes).sum
  }

  sealed trait Placement {
    def orientation: (Placement, Placement)
    def rotate90: Placement
    def matches(t1: Tile, t2: Tile): Boolean = t1.border(orientation._1) == t2.border(orientation._2)
  }
  case object Top extends Placement {
    override val orientation: (Placement, Placement) = (Top, Bottom)
    override val rotate90: Placement = Left
  }
  case object Bottom extends Placement {
    override val orientation: (Placement, Placement) = (Bottom, Top)
    override val rotate90: Placement = Right
  }
  case object Left extends Placement {
    override val orientation: (Placement, Placement) = (Left, Right)
    override val rotate90: Placement = Bottom
  }
  case object Right extends Placement {
    override val orientation: (Placement, Placement) = (Right, Left)
    override val rotate90: Placement = Top
  }
  case class Match(tile1: Tile, placement: Placement, tile2: Tile) {
    import Match.rotations

    if (!placement.matches(tile1, tile2)) throw new IllegalStateException(this.toString)

    override def toString: String = s"Match(${tile1.id} -> $placement -> ${tile2.id})"

    def rotate90: Match = Match(tile1.rotate90, placement.rotate90, tile2.rotate90)

    def rotate180: Match = rotate90.rotate90
    def rotate270: Match = rotate180.rotate90
    def flip: Match = {
      val flipped1 = tile1.flip
      val flipped2 = tile2.flip
      if (Set[Placement](Right, Left).contains(placement)) Match(flipped1, placement.orientation._2, flipped2)
      else Match(flipped1, placement, flipped2)
    }

    def orientTo(tile: Tile): Match = {
      rotations.map(_(this)).find(_.tile1 == tile).get
    }
  }

  case object Match {
    val rotations: LazyList[Match => Match] =
      LazyList(m => m, _.rotate90, _.rotate180, _.rotate270, _.flip, _.flip.rotate90, _.flip.rotate180, _.flip.rotate270)
  }

  object Tile {

    val idPattern: Regex = "Tile (\\d+):".r
  }

  def parse(input: List[String], acc: List[Tile] = List()): List[Tile] =
    if (input.isEmpty) acc
    else {
      val (tileLines, rest) = input.splitAt(11)
      val Tile.idPattern(id) = tileLines.head
      val rows = tileLines.tail.map(line => PixelLine(line.toVector)).toVector
      Tile(id.toLong, rows) :: parse(rest.drop(1), acc)
    }

  case class AllMatches(map: Map[Long, List[Match]], size: Int) {
    def corners: Map[Long, List[Match]] = map.filter(_._2.size <= 2)

    def reconstruct: Image = {
      val image = Image(Vector.fill(size, size)(null).map(ImageLine(_)))
      val (cornerId, cornerMatches) = corners.head
      val oriented = Match.rotations.map(rotation => cornerMatches.map(rotation(_))).find(_.map(_.placement).toSet == Set(Right, Bottom)).get.head
      val updatedMap = map.updated(cornerId, map(cornerId).map(_.orientTo(oriented.tile1)))
      _reconstruct(updatedMap, image.setTile(Pos(0, 0), oriented.tile1))
    }

    @tailrec
    private def _reconstruct(adjustedMap: Map[Long, List[Match]], acc: Image): Image = {
      val lastUpdated = acc.lastUpdated
      val (prevPos, pos, placement) =
        if (lastUpdated.x >= size - 1) (Pos(0, lastUpdated.y), Pos(0, lastUpdated.y + 1), Bottom)
        else (lastUpdated, Pos(lastUpdated.x + 1, lastUpdated.y), Right)
      if (pos.y >= size) acc
      else {
        val prevTile = acc.getTile(prevPos)
        val matchesToPrev = adjustedMap(prevTile.id).filter(_.placement == placement)
        val matchToPrev = matchesToPrev.head
        val tile = matchToPrev.tile2
        val updatedAcc = acc.setTile(pos, tile)
        val matchesForTile = adjustedMap(tile.id)
        val updatedMap = adjustedMap.updated(tile.id, matchesForTile.map(_.orientTo(tile)))
        _reconstruct(updatedMap, updatedAcc)
      }
    }
  }

  object AllMatches {

    def apply(tiles: List[Tile]): AllMatches = {
      val map = (for {
        t1 <- tiles
        t2 <- tiles
        if t1 != t2
      } yield {
        t1.calculateMatches(t2)
      }).flatten.groupBy(_.tile1.id)
      AllMatches(map, Math.sqrt(map.size).toInt)
    }
  }

  case class PixelLine(pixels: Vector[Char]) {

    def removeBorders(): PixelLine = PixelLine(pixels.tail.take(pixels.size - 2))

    def toImagePixels: String = pixels.mkString
    def reverse: PixelLine = PixelLine(pixels.reverse)
    def pixelAt(i: Int): Char = pixels(i)

    def countHashes: Int = pixels.count(_ == '#')

    def join(line: PixelLine): PixelLine = PixelLine(pixels ++ line.pixels)
  }

  case class Pos(x: Int, y: Int) {
    def +(pos: Pos): Pos = Pos(x + pos.x, y + pos.y)
  }
  case class ImageLine(tiles: Vector[Tile]) {

    def tileAt(i: Int): Tile = tiles(i)
    def setTileAt(i: Int, tile: Tile): ImageLine =
      if (tileAt(i) != null) throw new IllegalStateException
      else ImageLine(tiles.updated(i, tile))

    def removeBorders(): ImageLine = ImageLine(tiles.map(_.removeBorders()))

    def join(): Tile = tiles.reduce(_.join(_))

  }
  case class Image(rows: Vector[ImageLine], lastUpdated: Pos = Pos(0, 0)) {
    def join(): Tile = Tile(-1, rows.flatMap(_.join().rows))

    def getTile(pos: Pos): Tile = rows(pos.y).tileAt(pos.x)

    def setTile(pos: Pos, tile: Tile): Image = {
      val row = rows(pos.y)
      Image(rows.updated(pos.y, row.setTileAt(pos.x, tile)), lastUpdated = pos)
    }

    def removeBorders(): Image = Image(rows.map(_.removeBorders()))
  }

  object SeaMonster {

    val monsterShape: LazyList[Pos] = LazyList(
                                                                                                                         p(18,0),
      p(0,1),                        p(5,1),p(6,1),                        p(11,1),p(12,1),                      p(17,1),p(18,1),p(19,1),
              p(1,2),         p(4,2),              p(7,2),         p(10,2),                p(13,2),       p(16,2)
    )

    def p(x: Int, y: Int): Pos = Pos(x, y)

    def countMonsters(tile: Tile): Int =
      (for {
        x <- Range(0, tile.width)
        y <- tile.rows.indices
      } yield checkAt(Pos(x, y), tile)).sum

    def checkAt(pos: Day_20.Pos, tile: Day_20.Tile): Int =
      if (monsterShape.forall(p => isHash(pos + p, tile))) 1
      else 0

    private def isHash(pos: Pos, tile: Tile): Boolean =
      tile.isInside(pos) && tile.pixelAt(pos) == '#'
  }


  implicit class StringAsPixels(s: String) {

    def toPixels: List[Char] = s.toList
  }

  solve()
}
