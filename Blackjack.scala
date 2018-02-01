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
      case Ace => 1
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

  case class Hand(cards: List[Card]) {
    override def toString: String = cards match {
      case Card(r, s)::x::xs => s"$r$s  " + Hand(x::xs).toString
      case Card(r, s)::xs => s"$r$s" + Hand(xs).toString
      case Nil => ""
    }

    val score: Int = cards.foldRight(0)(_.rank.score + _)
    def scoreCategory: ScoreCategory = {
      if (this.score < 21) Normal
      else if (this.score == 21) Blackjack
      else TooHigh
    }
  }

  sealed trait ScoreCategory
  case object Blackjack extends ScoreCategory
  case object TooHigh extends ScoreCategory
  case object Normal extends ScoreCategory

  object Deck {
    val full = Deck(
      for {
        suite <- List(Diamonds, Hearts, Clubs, Spades)
        rank <- List(Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King)
      } yield Card(rank, suite)
    )

    def shuffledDeck: Deck = Deck(scala.util.Random.shuffle(full.cards))
  }

  val deck: Deck = Deck.shuffledDeck
  val hand: Hand = Hand(List.empty)

  val (firstHand, firstDeck) = dealCard(hand, deck)
  val (secondHand, secondDeck) = dealCard(firstHand, firstDeck)

  def dealCard(hand: Hand, deck: Deck): (Hand, Deck) = {
    val (cardOption, newDeck) = deck.dealCard()
    val newHand = Hand(cardOption.toList++hand.cards)

    (newHand, newDeck)
  }

  def play(hand: Hand, deck: Deck): (Hand, Deck) = {
    println(s"\n  Hand: $hand")
    println(s"  Score: ${hand.score}\n")
    println("  Would you like to draw a new card [y/n]? ")

    scala.io.StdIn.readLine() match {
      case "y" => {
        val (newHand, newDeck) = dealCard(hand, deck)

        newHand.scoreCategory match {
          case Normal => play(newHand, newDeck)
          case Blackjack => println("  <<< BLACKJACK! You WIN! >>>\n");
          case _ => println("  <<< Your hand is over 21! Game over >>>\n")
        }
      }
      case _ =>  println("  <<< Ok, no more cards for you. Game over >>>\n")
    }

    (hand, deck)
  }

  println("\n  ===========================================")
  println("  =                                         =")
  println("  =          Let's play Blackjack           =")
  println("  =                                         =")
  println("  ===========================================\n")

  println("  Drawing your first 2 cards...")
  play(secondHand, secondDeck)
}

