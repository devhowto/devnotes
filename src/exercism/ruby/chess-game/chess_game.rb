module Chess
  RANKS = 1..8
  FILES = 'A'..'H'

  def self.valid_square?(rank, file)
    RANKS.include?(rank) && FILES.include?(file)
  end

  def self.nick_name(first_name, last_name)
    ('%s%s' % [first_name[0..1], last_name[-2..-1]]).upcase
  end

  def self.move_message(first_name, last_name, square)
    file, rank = square.upcase.split('')
    nick = self.nick_name(first_name, last_name)

    if self.valid_square?(rank.to_i, file)
      '%s moved to %s' % [nick, square]
    else
      '%s attempted to move to %s, but that is not a valid square' %
        [nick, square]
    end
  end
end
