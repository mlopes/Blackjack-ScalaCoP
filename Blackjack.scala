import scala.io.StdIn

object Blackjack extends App {

  sealed trait Rank {
    override def toString: String = this match {
      case Ace => "A"
      case Two => "2"
      case Three => "3"
      case Four => "4"
      case Five => "5"
      case Six => "6"
      case Seven => "7"
      case Eight => "8"
      case Nine => "9"
      case Ten => "10"
      case Jack => "J"
      case Queen => "Q"
      case King => "K"
    }

    def score: Int = this match {
      case Ace => 11
      case Two => 2
      case Three => 3
      case Four => 4
      case Five => 5
      case Six => 6
      case Seven => 7
      case Eight => 8
      case Nine => 9
      case Ten => 10
      case Jack => 10
      case Queen => 10
      case King => 10
    }
  }

  sealed trait Suit {
    override def toString: String = this match {
      case Diamonds => "♢"
      case Hearts => "♡"
      case Clubs => "♧"
      case Spades => "♤"
    }
  }

  case class Card(rank: Rank, suit: Suit)

  case class Deck(cards: List[Card]) {
    def dealCard(): (Option[Card], Deck) = cards match {
      case x::xs => (Some(x), Deck(xs))
      case Nil => (None, Deck(List.empty))
    }
  }

  case object Ace extends Rank

  case object Two extends Rank

  case object Three extends Rank

  case object Four extends Rank

  case object Five extends Rank

  case object Six extends Rank

  case object Seven extends Rank

  case object Eight extends Rank

  case object Nine extends Rank

  case object Ten extends Rank

  case object Jack extends Rank

  case object Queen extends Rank

  case object King extends Rank

  case object Diamonds extends Suit

  case object Hearts extends Suit

  case object Clubs extends Suit

  case object Spades extends Suit

  sealed trait ScoreCategory
  case object Blackjack extends ScoreCategory
  case object TooHigh extends ScoreCategory
  case object Normal extends ScoreCategory

  case class Hand(cards: List[Card]) {
    override def toString: String = cards match {
      case Card(r, s)::x::xs => s"$r$s  " + Hand(x::xs).toString
      case Card(r, s)::xs => s"$r$s" + Hand(xs).toString
      case Nil => ""
    }

    val score: Int = {
      // simply add up all the card values
      val rawScore: Int = cards.foldRight(0)(_.rank.score + _)

      def optimizedScore(cards: List[Card], score: Int): Int = {
        if (score > 21) {
          cards match {
            // demote each Ace from 11 points to 1 point as long as the total is > 21 points
            case Card(Ace, _)::xs => optimizedScore(xs, score - 10)
            case x::xs => optimizedScore(xs, score)
            case Nil => score
          }
        } else score
      }

      optimizedScore(cards, rawScore)
    }

    val scoreCategory: ScoreCategory = {
      if (this.score < 21) Normal
      else if (this.score == 21) Blackjack
      else TooHigh
    }
  }

  object Deck {
    val full = Deck(
      for {
        suite <- List(Diamonds, Hearts, Clubs, Spades)
        rank <- List(Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King)
      } yield Card(rank, suite)
    )

    def shuffledDeck: Deck = Deck(scala.util.Random.shuffle(full.cards))
  }

  def dealCard(hand: Hand, deck: Deck): (Hand, Deck) = {
    val (cardOption, newDeck) = deck.dealCard()
    val newHand = Hand(cardOption.toList++hand.cards)

    (newHand, newDeck)
  }

  def processHand(hand: Hand, deck: Deck): Unit = {
    println(s"\n  Hand: $hand\n  Score: ${hand.score}\n")

    hand.scoreCategory match {
      case Normal => playNewHand(hand, deck)
      case Blackjack => println("  <<< BLACKJACK! You WIN! >>>\n"); asktoPlayAgain
      case TooHigh => println("  <<< Your hand is over 21! Game over. >>>\n"); asktoPlayAgain
    }
  }

  def asktoPlayAgain: Unit = {
    val input = StdIn.readLine("\n  Would you like to play again [Y/n]? ")

    if (isInputYes(input)) newGame
    else println("  Thank you for playing. Bye, bye.")
  }

  def playNewHand(hand: Hand, deck: Deck): Unit = {
    val input: String = StdIn.readLine("  Would you like to draw a new card [Y/n]? ")

    if (isInputYes(input)) {
      val (newHand, newDeck) = dealCard(hand, deck)
      processHand(newHand, newDeck)
    } else println("  <<< Ok, no more cards for you. Game over >>>\n")
  }

  def isInputYes(input: String): Boolean = List("y", "Y", "").find(_ == input) match {
    case Some(_) => true
    case None => false
  }

  def printIntroMessage: Unit = {
    println("\n  ===========================================")
    println("  =                                         =")
    println("  =          Let's play Blackjack           =")
    println("  =                                         =")
    println("  ===========================================\n")

    println("  Drawing your first 2 cards...")
  }

  def newGame = {
    val (firstHand, firstDeck) = dealCard(Hand(List.empty), Deck.shuffledDeck)
    val (secondHand, secondDeck) = dealCard(firstHand, firstDeck)

    printIntroMessage
    processHand(secondHand, secondDeck)
  }

  newGame
}

