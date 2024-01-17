module Blackjack
  CARDS = %w[
    two
    three
    four
    five
    six
    seven
    eight
    nine
    ten
  ].map.with_index { |e, i| [e, i + 2] }.to_h.merge({
    'jack' => 10,
    'king' => 10,
    'queen' => 10,
    'ace' => 11,
  })

  def self.parse_card(card)
    CARDS[card] || 0
  end

  def self.card_range(card1, card2)
    case parse_card(card1) + parse_card(card2)
    when (4..11) then 'low'
    when (12..16) then 'mid'
    when (17..20) then 'high'
    when 21 then 'blackjack'
    else 'aces'
    end
  end

  def self.first_turn(card1, card2, dealer_card)
    case card_range(card1, card2)
    when 'aces'
      then 'P'
    when 'blackjack'
      parse_card(dealer_card) < 10 ? 'W' : 'S'
    when 'high'
      'S'
    when 'mid'
      parse_card(dealer_card) < 7 ? 'S' : 'H'
    else
      'H'
    end
  end
end
