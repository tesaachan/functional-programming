package objsets

import TweetReader.*

/**
 * A class to represent tweets.
 */
class Tweet(val user: String, val text: String, val retweets: Int):
  override def toString: String =
    "User: " + user + "\n" +
    "Text: " + text + " [" + retweets + "]"

/**
 * This represents a set of objects of type `Tweet` in the form of a binary search
 * tree. Every branch in the tree has two children (two `TweetSet`s). There is an
 * invariant which always holds: for every branch `b`, all elements in the left
 * subtree are smaller than the tweet at `b`. The elements in the right subtree are
 * larger.
 *
 * Note that the above structure requires us to be able to compare two tweets (we
 * need to be able to say which of two tweets is larger, or if they are equal). In
 * this implementation, the equality / order of tweets is based on the tweet's text
 * (see `def incl`). Hence, a `TweetSet` could not contain two tweets with the same
 * text from different users.
 */
abstract class TweetSet extends TweetSetInterface:

  /**
   * This method takes a predicate and returns a subset of all the elements
   * in the original set for which the predicate is true.
   */
  def filter(p: Tweet => Boolean): TweetSet = this.filterAcc(p, new Empty)

  /**
   * This is a helper method for `filter` that propagates the accumulated tweets.
   */
  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet

  /**
   * Returns a new `TweetSet` that is the union of `TweetSet`s `this` and `that`.
   */
  def union(that: TweetSet): TweetSet

  /**
   * Returns the tweet from this set which has the greatest retweet count.
   *
   * Calling `mostRetweeted` on an empty set should throw an exception of
   * type `java.util.NoSuchElementException`.
   */
  def mostRetweeted: Tweet

  def mostRetweetedAcc(most: Tweet): Tweet

  /**
   * Returns a list containing all tweets of this set, sorted by retweet count
   * in descending order. In other words, the head of the resulting list should
   * have the highest retweet count.
   */
  def descendingByRetweet: TweetList

  /**
   * Returns a new `TweetSet` which contains all elements of this set, and the
   * the new element `tweet` in case it does not already exist in this set.
   *
   * If `this.contains(tweet)`, the current set is returned.
   */
  def incl(tweet: Tweet): TweetSet

  /**
   * Returns a new `TweetSet` which excludes `tweet`.
   */
  def remove(tweet: Tweet): TweetSet

  /**
   * Tests if `tweet` exists in this `TweetSet`.
   */
  def contains(tweet: Tweet): Boolean

  /**
   * This method takes a function and applies it to every element in the set.
   */
  def foreach(f: Tweet => Unit): Unit


class Empty extends TweetSet:
  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = acc

  def union(that: TweetSet): TweetSet = that

  def mostRetweeted: Tweet = throw new java.util.NoSuchElementException

  def mostRetweetedAcc(most: Tweet): Tweet = most

  def descendingByRetweet: TweetList = Nil

  def contains(tweet: Tweet): Boolean = false

  def incl(tweet: Tweet): TweetSet = NonEmpty(tweet, Empty(), Empty())

  def remove(tweet: Tweet): TweetSet = this

  def foreach(f: Tweet => Unit): Unit = ()


class NonEmpty(elem: Tweet, left: TweetSet, right: TweetSet) extends TweetSet:

  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet =
    left.filterAcc(p, right.filterAcc(p, if p(this.elem) then acc.incl(elem) else acc))

  def descendingByRetweet: TweetList = 
    new Cons(this.mostRetweeted, this.remove(this.mostRetweeted).descendingByRetweet)

  def union(that: TweetSet): TweetSet =
    left.union((right).union(that)).incl(this.elem)

  def mostRetweeted: Tweet = 
    mostRetweetedAcc(this.elem)
  
  def mostRetweetedAcc(most: Tweet): Tweet =
    val max: Tweet = if this.elem.retweets > most.retweets then this.elem else most
    val left_most: Tweet = this.left.mostRetweetedAcc(max)
    val right_most: Tweet = this.right.mostRetweetedAcc(max)
    if left_most.retweets > right_most.retweets then left_most
    else right_most

  def contains(x: Tweet): Boolean =
    if x.text < elem.text then
      left.contains(x)
    else if elem.text < x.text then
      right.contains(x)
    else true

  def incl(x: Tweet): TweetSet =
    if x.text < elem.text then
      NonEmpty(elem, left.incl(x), right)
    else if elem.text < x.text then
      NonEmpty(elem, left, right.incl(x))
    else
      this

  def remove(tw: Tweet): TweetSet =
    if tw.text < elem.text then
      NonEmpty(elem, left.remove(tw), right)
    else if elem.text < tw.text then
      NonEmpty(elem, left, right.remove(tw))
    else
      left.union(right)

  def foreach(f: Tweet => Unit): Unit =
    f(elem)
    left.foreach(f)
    right.foreach(f)

trait TweetList:
  def head: Tweet
  def tail: TweetList
  def isEmpty: Boolean
  def foreach(f: Tweet => Unit): Unit =
    if !isEmpty then
      f(head)
      tail.foreach(f)

object Nil extends TweetList:
  def head = throw java.util.NoSuchElementException("head of EmptyList")
  def tail = throw java.util.NoSuchElementException("tail of EmptyList")
  def isEmpty = true

class Cons(val head: Tweet, val tail: TweetList) extends TweetList:
  def isEmpty = false


object GoogleVsApple:
  val google = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus")
  val apple = List("ios", "iOS", "iphone", "iPhone", "ipad", "iPad")

  lazy val googleTweets: TweetSet =
    def find(x: Tweet): Boolean = google.exists((y: String) => x.text contains y)
    TweetReader.allTweets.filter(find)

  lazy val appleTweets: TweetSet = 
    def find(x: Tweet): Boolean = apple.exists((y: String) => x.text contains y)
    TweetReader.allTweets.filter(find)

  /**
   * A list of all tweets mentioning a keyword from either apple or google,
   * sorted by the number of retweets.
   */
  lazy val trending: TweetList = googleTweets.union(appleTweets).descendingByRetweet


object Main extends App:
  // Print the trending tweets
  GoogleVsApple.trending foreach println
