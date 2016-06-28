package graph

import models.{Point, Box}
import scala.collection.mutable.{ArrayBuffer, PriorityQueue}

object Constants {
	// $COVERAGE-OFF$
	@inline final val MaxEntries = 50
	// $COVERAGE-ON$
}

import Constants._

trait HasGeom {
	def geom: Geom
}

case class Entry[A](geom: Geom, value: A) extends HasGeom

sealed abstract class Node[A] extends HasGeom { self =>

	def box: Box
	def geom: Geom = box

	def children: Vector[HasGeom]

	def entries: Vector[Entry[A]] = {
		val buf = ArrayBuffer.empty[Entry[A]]
		def recur(node: Node[A]): Unit = node match {
			case Leaf(children, _) =>
				buf ++= children
			case Branch(children, _) =>
				children.foreach(recur)
		}
		recur(this)
		buf.toVector
	}

	def iterator: Iterator[Entry[A]] = this match {
		case Leaf(children, _) =>
			children.iterator
		case Branch(children, _) =>
			children.iterator.flatMap(_.iterator)
	}

	def pretty: String = {
		def prettyRecur(node: Node[A], i: Int, sb: StringBuilder): Unit = {
			val pad = " " * i
			val a = node.box.area
			node match {
				case lf @ Leaf(children, box) =>
					val pad2 = " " * (i + 1)
					sb.append(s"$pad leaf $a $box:\n")
					children.foreach { case Entry(pt, value) =>
						sb.append(s"$pad2 entry $pt: $value\n")
					}
				case Branch(children, box) =>
					sb.append(s"$pad branch $a $box:\n")
					children.foreach(c => prettyRecur(c, i + 1, sb))
			}
		}
		val sb = new StringBuilder
		prettyRecur(this, 0, sb)
		sb.toString
	}

	def insert(entry: Entry[A]): Either[Vector[Node[A]], Node[A]] = {
		this match {
			case Leaf(children, box) =>
				val cs = children :+ entry
				if (cs.length <= MaxEntries) {
					Right(Leaf(cs, box.expand(entry.geom)))
				} else {
					Left(Node.splitLeaf(cs))
				}
			case Branch(children, box) => assert(children.nonEmpty)

				val pt = entry.geom
				var node = children(0)
				var n = 0
				var area = node.box.expandArea(pt)
				var i = 1
				while (i < children.length) {
					val curr = children(i)
					val a = curr.box.expandArea(pt)
					if (a < area) {
						area = a
						n = i
						node = curr
					}
					i += 1
				}
				node.insert(entry) match {
					case Left(rs) =>
						val cs = children.take(n) ++ children.drop(n + 1) ++ rs
						if (cs.length <= MaxEntries) {
							val b = rs.foldLeft(box)(_ expand _.box)
							Right(Branch(cs, b))
						} else {
							Left(Node.splitBranch(cs))
						}
					case Right(r) =>
						val cs = children.updated(n, r)
						if (cs.length <= MaxEntries) {
							Right(Branch(children.updated(n, r), box.expand(r.box)))
						} else {
							Left(Node.splitBranch(cs))
						}
				}
		}
	}

	def contract(gone: Geom, regen: => Box): Box =
		if (box.wraps(gone)) box else regen

	def remove(entry: Entry[A]): Option[(Joined[Entry[A]], Option[Node[A]])]

	def search(space: Box, f: Entry[A] => Boolean): Seq[Entry[A]] =
		genericSearch(space, space.contains, f)

	def searchIntersection(space: Box, f: Entry[A] => Boolean): Seq[Entry[A]] =
		genericSearch(space, space.intersects, f)

	def genericSearch(space: Box, check: Geom => Boolean, f: Entry[A] => Boolean): Seq[Entry[A]] =
		if (!space.isFinite) Nil else {
			val buf = ArrayBuffer.empty[Entry[A]]
			def recur(node: Node[A]): Unit = node match {
				case Leaf(children, box) =>
					children.foreach { c =>
						if (check(c.geom) && f(c)) buf.append(c)
					}
				case Branch(children, box) =>
					children.foreach { c =>
						if (space.intersects(box)) recur(c)
					}
			}
			if (space.intersects(box)) recur(this)
			buf
		}

	def foldSearch[B](space: Box, init: B)(f: (B, Entry[A]) => B): B =
		searchIterator(space, _ => true).foldLeft(init)(f)

	def searchIterator(space: Box, f: Entry[A] => Boolean): Iterator[Entry[A]] =
		if (children.isEmpty || !box.intersects(space)) {
			Iterator.empty
		} else {
			this match {
				case Leaf(cs, _) =>
					cs.iterator.filter(c => space.contains(c.geom) && f(c))
				case Branch(cs, _) =>
					cs.iterator.flatMap(c => c.searchIterator(space, f))
			}
		}

	def nearest(pt: Point, d0: Double): Option[(Double, Entry[A])] = {
		var dist: Double = d0
		var result: Option[(Double, Entry[A])] = None
		this match {
			case Leaf(children, box) =>
				children.foreach { entry =>
					val d = entry.geom.distance(pt)
					if (d < dist) {
						dist = d
						result = Some((d, entry))
					}
				}
			case Branch(children, box) =>
				val cs = children.map(node => (node.box.distance(pt), node)).sortBy(_._1)
				cs.foreach { case (d, node) =>
					if (d >= dist) return result //scalastyle:ignore
					node.nearest(pt, dist) match {
						case some @ Some((d, _)) =>
							dist = d
							result = some
						case None =>
					}
				}
		}
		result
	}
	def nearestK(pt: Point, k: Int, d0: Double, pq: PriorityQueue[(Double, Entry[A])]): Double = {
		var dist: Double = d0
		this match {
			case Leaf(children, box) =>
				children.foreach { entry =>
					val d = entry.geom.distance(pt)
					if (d < dist) {
						pq += ((d, entry))
						if (pq.size > k) {
							pq.dequeue
							dist = pq.head._1
						}
					}
				}
			case Branch(children, box) =>
				val cs = children.map(node => (node.box.distance(pt), node)).sortBy(_._1)
				cs.foreach { case (d, node) =>
					if (d >= dist) return dist
					dist = node.nearestK(pt, k, dist, pq)
				}
		}
		dist
	}
	def count(space: Box): Int =
		if (!space.isFinite) 0 else {
			def recur(node: Node[A]): Int = node match {
				case Leaf(children, box) =>
					var n = 0
					var i = 0
					while (i < children.length) {
						if (space.contains(children(i).geom)) n += 1
						i += 1
					}
					n
				case Branch(children, box) =>
					var n = 0
					var i = 0
					while (i < children.length) {
						val c = children(i)
						if (space.intersects(c.box)) n += recur(c)
						i += 1
					}
					n
			}
			if (space.intersects(box)) recur(this) else 0
		}
	def contains(entry: Entry[A]): Boolean =
		searchIterator(entry.geom.toBox, _ == entry).nonEmpty

	def map[B](f: A => B): Node[B] = this match {
		case Leaf(cs, box) =>
			Leaf(cs.map(e => Entry(e.geom, f(e.value))), box)
		case Branch(cs, box) =>
			Branch(cs.map(_.map(f)), box)
	}
}

object Node {

	def empty[A]: Node[A] = Leaf(Vector.empty, Box.empty)

	def splitLeaf[A](children: Vector[Entry[A]]): Vector[Leaf[A]] = {
		val ((es1, box1), (es2, box2)) = splitter(children)
		Vector(Leaf(es1, box1), Leaf(es2, box2))
	}

	def splitBranch[A](children: Vector[Node[A]]): Vector[Branch[A]] = {
		val ((ns1, box1), (ns2, box2)) = splitter(children)
		Vector(Branch(ns1, box1), Branch(ns2, box2))
	}

	def splitter[M <: HasGeom](children: Vector[M]): ((Vector[M], Box), (Vector[M], Box)) = {
		val buf = ArrayBuffer(children: _*)
		val (seed1, seed2) = pickSeeds(buf)

		var box1: Box = seed1.geom.toBox
		var box2: Box = seed2.geom.toBox
		val nodes1 = ArrayBuffer(seed1)
		val nodes2 = ArrayBuffer(seed2)

		def add1(node: M): Unit = { nodes1 += node; box1 = box1.expand(node.geom) }
		def add2(node: M): Unit = { nodes2 += node; box2 = box2.expand(node.geom) }

		while (buf.nonEmpty) {

			if (nodes1.length >= 2 && nodes2.length + buf.length <= 2) {
				nodes2 ++= buf
				box2 = buf.foldLeft(box2)(_ expand _.geom)
				buf.clear()

			} else if (nodes2.length >= 2 && nodes1.length + buf.length <= 2) {
				nodes1 ++= buf
				box1 = buf.foldLeft(box1)(_ expand _.geom)
				buf.clear()

			} else {
				val node = buf.remove(buf.length - 1)
				val e1 = box1.expandArea(node.geom)
				val e2 = box2.expandArea(node.geom)
				if (e1 < e2) {
					add1(node)
				} else if (e2 < e1) {
					add2(node)
				} else {
					val b1 = box1.expand(node.geom)
					val b2 = box2.expand(node.geom)
					val a1 = b1.area
					val a2 = b2.area
					if (a1 < a2) {
						add1(node)
					} else if (a2 < a1) {
						add2(node)
					} else if (Math.random() > 0.5) {
						add1(node)
					} else {
						add2(node)
					}
				}
			}
		}
		((nodes1.toVector, box1), (nodes2.toVector, box2))
	}

	def pickSeeds[M <: HasGeom](nodes: ArrayBuffer[M]): (M, M) = {

		def handleDimension(pairs: IndexedSeq[(Double, Double)]): (Double, Int, Int) = {
			val (a0, b0) = pairs(0)
			var amin = a0 // min lower coord
			var amax = a0 // max lower coord
			var bmin = b0 // min upper coord
			var bmax = b0 // max upper coord

			var left = 0
			var right = 0
			var i = 1
			while (i < pairs.length) {
				val (a, b) = pairs(i)
				if (a < amin) { amin = a }
				if (a > amax) { amax = a; right = i }
				if (b > bmax) { bmax = b }
				if (b < bmin) { bmin = b; left = i }
				i += 1
			}

			if (left != right) ((bmin - amax) / (bmax - amin), left, right) else (0.0, 0, 1)
		}
		val (w1, i1, j1) = handleDimension(nodes.map(n => (n.geom.x, n.geom.x2)))
		val (w2, i2, j2) = handleDimension(nodes.map(n => (n.geom.y, n.geom.y2)))

		val (i, j) = if (w1 > w2) (i1, j1) else (i2, j2)

		val (a, b) = if (i > j) (i, j) else (j, i)
		val node1 = nodes.remove(a)
		val node2 = nodes.remove(b)
		(node1, node2)
	}
}

case class Leaf[A](children: Vector[Entry[A]], box: Box) extends Node[A] {

	def remove(entry: Entry[A]): Option[(Joined[Entry[A]], Option[Node[A]])] = {
		if (!box.contains(entry.geom)) return None
		val i = children.indexOf(entry)
		if (i < 0) {
			None
		} else if (children.length == 1) {
			Some((Joined.empty[Entry[A]], None))
		} else if (children.length == 2) {
			Some((Joined(children(1 - i)), None))
		} else {
			val cs = children.take(i) ++ children.drop(i + 1)
			val b = contract(entry.geom, cs.foldLeft(Box.empty)(_ expand _.geom))
			Some((Joined.empty[Entry[A]], Some(Leaf(cs, b))))
		}
	}
}

case class Branch[A](children: Vector[Node[A]], box: Box) extends Node[A] {

	def remove(entry: Entry[A]): Option[(Joined[Entry[A]], Option[Node[A]])] = {
		def loop(i: Int): Option[(Joined[Entry[A]], Option[Node[A]])] =
			if (i < children.length) {
				val child = children(i)
				child.remove(entry) match {
					case None =>
						loop(i + 1)

					case Some((es, None)) =>
						if (children.length == 1) {
							Some((es, None))
						} else if (children.length == 2) {
							Some((Joined.wrap(children(1 - i).entries) ++ es, None))
						} else {
							val cs = children.take(i) ++ children.drop(i + 1)
							val b = contract(child.geom, cs.foldLeft(Box.empty)(_ expand _.geom))
							Some((es, Some(Branch(cs, b))))
						}

					case Some((es, Some(node))) =>
						val cs = children.updated(i, node)
						val b = contract(child.geom, cs.foldLeft(Box.empty)(_ expand _.geom))
						Some((es, Some(Branch(cs, b))))
				}
			} else {
				None
			}

		if (!box.contains(entry.geom)) None else loop(0)
	}
}

