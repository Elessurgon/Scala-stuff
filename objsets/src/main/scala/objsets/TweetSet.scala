package objsets

class Tweet(val user: String, val text: String, val retweets: Int) {
  override def toString: String =
    "User: " + user + "\n" +
      "Text: " + text + " [" + retweets + "]"
}

abstract class TweetSet extends TweetSetInterface {

  def filter(p: Tweet => Boolean): TweetSet = filterAcc(p, new Empty)

  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet

  def union(that: TweetSet): TweetSet = filterAcc(tweet => true, that)

  def mostRetweeted: Tweet

  def descendingByRetweet: TweetList

  def incl(tweet: Tweet): TweetSet

  def remove(tweet: Tweet): TweetSet

  def contains(tweet: Tweet): Boolean

  def foreach(f: Tweet => Unit): Unit

  def isEmpty: Boolean
}

class Empty extends TweetSet {
  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = acc

  def contains(tweet: Tweet): Boolean = false

  def incl(tweet: Tweet): TweetSet = new NonEmpty(tweet, new Empty, new Empty)

  def remove(tweet: Tweet): TweetSet = this

  def foreach(f: Tweet => Unit): Unit = ()

  def isEmpty: Boolean = true

  def mostRetweeted: Tweet = throw new java.util.NoSuchElementException()

  def descendingByRetweet: TweetList = Nil

  override def toString: String = "X"
}

class NonEmpty(elem: Tweet, left: TweetSet, right: TweetSet) extends TweetSet {

  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = {
    val children = right.filterAcc(p, left.filterAcc(p, acc))
    if (p(elem)) children.incl(elem)
    else children
  }

  def contains(x: Tweet): Boolean =
    if (x.text < elem.text) left.contains(x)
    else if (elem.text < x.text) right.contains(x)
    else true

  def incl(x: Tweet): TweetSet = {
    if (x.text < elem.text) new NonEmpty(elem, left.incl(x), right)
    else if (elem.text < x.text) new NonEmpty(elem, left, right.incl(x))
    else this
  }

  def remove(tw: Tweet): TweetSet =
    if (tw.text < elem.text) new NonEmpty(elem, left.remove(tw), right)
    else if (elem.text < tw.text) new NonEmpty(elem, left, right.remove(tw))
    else left.union(right)

  def foreach(f: Tweet => Unit): Unit = {
    f(elem)
    left.foreach(f)
    right.foreach(f)
  }

  override def toString: String = {
    "{" + elem.toString + ", " +
      left.toString + ", " +
      right.toString + "}"
  }

  def mostRetweeted: Tweet = {
    def findMax(tweet1: Tweet, tweet2: Tweet): Tweet = {
      if (tweet1.retweets > tweet2.retweets) tweet1
      else tweet2
    }

    if (left.isEmpty && right.isEmpty) elem
    else if (left.isEmpty) findMax(right.mostRetweeted, elem)
    else if (right.isEmpty) findMax(elem, left.mostRetweeted)
    else findMax(findMax(left.mostRetweeted, elem), right.mostRetweeted)

  }

  def descendingByRetweet: TweetList = {
    val tweet = mostRetweeted
    new Cons(tweet, remove(tweet).descendingByRetweet)
  }

  def isEmpty: Boolean = false
}

trait TweetList {
  def head: Tweet

  def tail: TweetList

  def isEmpty: Boolean

  def foreach(f: Tweet => Unit): Unit =
    if (!isEmpty) {
      f(head)
      tail.foreach(f)
    }

}

object Nil extends TweetList {
  def head = throw new java.util.NoSuchElementException("head of EmptyList")

  def tail = throw new java.util.NoSuchElementException("tail of EmptyList")

  def isEmpty = true
}


class Cons(val head: Tweet, val tail: TweetList) extends TweetList {
  def isEmpty = false
}


object GoogleVsApple {
  val google = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus")
  val apple = List("ios", "iOS", "iphone", "iPhone", "ipad", "iPad")

  def filterTweetbyTags(tags: List[String]): TweetSet = {
    TweetReader.allTweets.filter(tweet => tags.exists(word => tweet.text.contains(word)))
  }

  lazy val googleTweets: TweetSet = filterTweetbyTags(google)
  lazy val appleTweets: TweetSet = filterTweetbyTags(apple)

  /**
   * A list of all tweets mentioning a keyword from either apple or google,
   * sorted by the number of retweets.
   */
  lazy val trending: TweetList = googleTweets.union(appleTweets).descendingByRetweet
}

object Main extends App {
  // Print the trending tweets
  GoogleVsApple.trending foreach println
}
