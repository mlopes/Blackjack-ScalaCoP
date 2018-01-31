object Blackjack extends App {

  sealed trait Rank

  sealed trait Suit

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

  case class Hand(cards: List[Card])

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
    println(hand)
    println("Would you like to draw a card? []")
    scala.io.StdIn.readLine() match {
      case "yes" => {
        val (newHand, newDeck) = dealCard(hand, deck)

        play(newHand, newDeck)
      }
      case _ => println("Game over. You should see your score now."); (hand, deck)
    }
  }

  play(secondHand, secondDeck)
}

