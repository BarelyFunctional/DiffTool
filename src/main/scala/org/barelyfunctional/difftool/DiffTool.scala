package org.barelyfunctional.difftool

import scala.annotation.tailrec
import scala.xml._
import scala.Some
import scala.xml.Comment

object MapUtils {
  def sameKeysDifferentValues[K, V](lhs : Map[K, V], rhs : Map[K, V]) : Map[K, (V, V)] =
   lhs.keySet.intersect(rhs.keySet).collect {
      case key if lhs(key) != rhs(key) => (key, (lhs(key), rhs(key)))
    } toMap
}

object TreeDiff {

  trait Diff

  type DiffPath = List[BranchDifference[_]]
  type AbsoluteDiff = (DiffPath, LeafDiff)

  def allDiffs(diff : Diff) : List[AbsoluteDiff] =
    diff match {
      case leaf : LeafDiff => List((List.empty[BranchDifference[_]], leaf))
      case branch : BranchDifference[_] =>
        branch.childDiffs.flatMap(allDiffs).map {
          case (path, leaf) => (branch :: path, leaf)
        }.toList
    }

  trait Side
  case object LeftHandSide extends Side
  case object RightHandSide extends Side


  trait Difference[+A] extends Diff {
    val lhs : A
    val rhs : A

    def isEmpty = lhs == rhs
  }

  trait LeafDiff extends Diff

  case class Insertion[A](inserted : A, insertedOn : Side) extends LeafDiff

  trait LeafDifference[+A] extends LeafDiff with Difference[A] {
    override def toString = lhs + " != " + rhs
  }

  trait BranchDifference[A] extends Difference[A] {
    def branchName : String
    def childDiffs : Seq[Diff]
  }

  case class NoDifference[A](lhs : A, rhs : A) extends LeafDifference[A] {
    override def isEmpty = true
  }

  @tailrec
  def sequenceDiff[A](lhs : List[A],
                      rhs : List[A],
                      differ : (A, A) => Difference[Any],
                      accumulator : List[Option[Diff]] = List.empty) : List[Option[Diff]] = {

    def toInsertions(inserted : List[A], insertedOn : Side) =
      inserted.map(i => Some(Insertion(i, insertedOn)))

    def equal(l : A, r : A) = differ(l, r).isEmpty

    (lhs, rhs) match {
      case (Nil, Nil) => accumulator.reverse
      case (Nil, r) => accumulator.reverse ++ toInsertions(r, RightHandSide)
      case (l, Nil) => accumulator.reverse ++ toInsertions(l, LeftHandSide)
      case (lHead :: lTail, rHead :: rTail) =>
        differ(lHead, rHead) match {
          case NoDifference(_, _) => sequenceDiff(lTail, rTail, differ, None :: accumulator)
          case diff =>
            (lTail.span(!equal(rHead, _)), rTail.span(!equal(lHead, _))) match {
              case ((_, Nil), (_, Nil)) => sequenceDiff(lTail, rTail, differ, Some(diff) :: accumulator)
              case ((lInserted, lRest), (rInserted, rRest)) =>
                if (lInserted.size < rInserted.size)
                  sequenceDiff(lRest, rhs, differ, toInsertions(lInserted, LeftHandSide) ++ accumulator)
                else
                  sequenceDiff(lhs, rRest, differ, toInsertions(rInserted, RightHandSide) ++ accumulator)
            }
        }
    }
  }

  def indices(diffs : Seq[Option[Diff]]) : Seq[(Int, Int)] =
    diffs.scanLeft((0, 0)) {
      case ((l, r), Some(Insertion(_, LeftHandSide))) => (l + 1, r)
      case ((l, r), Some(Insertion(_, RightHandSide))) => (l, r + 1)
      case ((l, r), _) => (l + 1, r + 1)
    }

}

object XmlDiff {

  import TreeDiff._

  def pretty(diff : AbsoluteDiff) =
    diff match {
 //     case (path, leaf : TagNameDifference) => path.init.map(_.branchName).mkString(" / ") + ": " + leaf
      case (path, leaf) => path.map(_.branchName).mkString(" / ") + ": " + leaf
    }

  case class TagNameDifference(lhs : String, rhs : String) extends LeafDifference[String] {
    override def toString = "Tag name is different: " + super.toString
  }

  case class Attribute(key : String, value : String)

  case class TextDiff(lhs : Text, rhs : Text) extends LeafDifference[Text] {
    override def toString = "Text is different: " + super.toString
  }

  case class CommentDiff(lhs : Comment, rhs : Comment) extends LeafDifference[Comment]

  case class TypeDiff(lhs : NodeSeq, rhs : NodeSeq) extends LeafDifference[NodeSeq]

  case class AttributeDifference(key : String, lhs : String, rhs : String) extends LeafDifference[String]

  type Attributes = Map[String, String]

  def attributesDifference(lhs : Attributes, rhs : Attributes) = {
    def newAttributes(attributes : Attributes, side : Side) : List[Insertion[Attribute]] =
      attributes.toList.map {
        case (key, value) => Insertion(Attribute(key, value), side)
      }

    newAttributes(lhs -- rhs.keySet, LeftHandSide) ++
    newAttributes(rhs -- lhs.keySet, RightHandSide) ++
      (MapUtils.sameKeysDifferentValues(lhs, rhs) map {
      case (key, (left, right)) => AttributeDifference(key, left, right)
    })
  }

  case class ElementDifference(lhs : Elem, rhs : Elem) extends BranchDifference[Elem] {

//    def tagNameDifference : Option[TagNameDifference] =
//      if (lhs.label == rhs.label) None else Some(TagNameDifference(lhs.label, rhs.label))

    def attributes(elem : Elem) = elem.attributes.asAttrMap

    def attributeDiffs = attributesDifference(attributes(lhs), attributes(rhs))

    def childNodeDiffs = sequenceDiff(lhs.child.toList, rhs.child.toList, XmlDiff.diff).flatten
    def childDiffs =
//      tagNameDifference.toList ++
      attributeDiffs ++ sequenceDiff(lhs.child.toList, rhs.child.toList, XmlDiff.diff).flatten

    def branchName = if (lhs.label == rhs.label) lhs.label else "(" + lhs.label + "," + rhs.label + ")"
  }

  def diff(lhs : NodeSeq, rhs : NodeSeq) : Difference[Any] = {
    if (lhs != rhs) {
      val different = (lhs, rhs) match {
        case (l : Elem, r : Elem) if l.label != r.label => TagNameDifference(l.label, r.label)
        case (l : Elem, r : Elem) => ElementDifference(l, r)
        case (l : Text, r : Text) => TextDiff(l, r)
        case (l : Comment, r : Comment) => CommentDiff(l, r)
        case _ if lhs.getClass != rhs.getClass => TypeDiff(lhs, rhs)
      }

      if (different.isEmpty) NoDifference(lhs, rhs) else different
    }
    else NoDifference(lhs, rhs)
  }
}
