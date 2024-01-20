##
# Improvements made from code review and mentoring from the @kotp.
##

module SimpleCalculatorExceptions
  class UnsupportedOperation < ArgumentError
    def initialize(message = 'A valid operation must be provided.')
      super
    end
  end

  class IntegerOperandError < ArgumentError
    def initialize(message = 'Operands must be integers.')
      super
    end
  end
end

class SimpleCalculator
  include SimpleCalculatorExceptions

  attr_reader :operand1, :operand2, :operator

  OP = OPERATE = {
    '+' => ->(operand1, operand2) { operand1 + operand2 },
    '*' => ->(operand1, operand2) { operand1 * operand2 },
    '/' => ->(dividend, divisor) { dividend / divisor },
  }

  OPS = ALLOWED_OPERATIONS = OP.keys

  private_constant :OP, :OPS

  ##
  # Applies the operator to the operands and returns a string
  # representing the entire expression with the answer or an
  # error message if some invalid input is provided.
  #
  # @param operand1 [Integer]
  # @param operand2 [Integer]
  # @param operator ['+', '*', '/']
  #
  def self.calculate(operand1, operand2, operator)
    calculation = new(operand1, operand2, operator)
    calculation.send :validate
    calculation.send :to_s
  end

  def initialize(operand1, operand2, operator)
    @operand1, @operand2, @operator = operand1, operand2, operator
  end

  def to_s
    run
  end

  private

  def run
    '%i %s %i = %i' %
      [
        operand1,
        operator,
        operand2,
        OP[operator].call(operand1, operand2),
      ]
  rescue ZeroDivisionError
    'Division by zero is not allowed.'
  end


  def validate
    raise IntegerOperandError unless
      [operand1, operand2].all? { |operand| operand.is_a?(Integer) }

    raise UnsupportedOperation unless OPS.member?(operator)
  end
end

if $PROGRAM_NAME == __FILE__
  puts SimpleCalculator.calculate(1, 2, '+')
  puts SimpleCalculator.new(1, 2, '+')
  p SimpleCalculator.new(1, 2, '+').to_s
end
