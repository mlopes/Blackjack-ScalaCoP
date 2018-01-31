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
    println("Your current hand is:")
    println(hand.toString)
    println("Would you like to draw a new card? yes/no")
    scala.io.StdIn.readLine() match {
      case "yes" => {
        val (newHand, newDeck) = dealCard(hand, deck)

        play(newHand, newDeck)
      }
      case _ => println("Game over."); (hand, deck)
    }
  }

  play(secondHand, secondDeck)
}

