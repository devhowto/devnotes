##
# Improvements made from code review and mentoring from the @kotp.
##

module SimpleCalculatorExceptions
  class UnsupportedOperation < ArgumentError
    def initialize(message = 'A valid operation must be provided.')
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
    new(operand1, operand2, operator).to_s
  end

  def initialize(operand1, operand2, operator)
    @operand1, @operand2, @operator = operand1, operand2, operator

    validate
    run
  end

  def run
    @result = '%i %s %i = %i' %
      [
        operand1,
        operator,
        operand2,
        OP[operator].call(operand1, operand2),
      ]

  rescue ZeroDivisionError => err
    @report = 'Division by zero is not allowed.'
  end

  def validate
    raise ArgumentError, 'Operands must be integers.' unless
      [operand1, operand2].all? { |operand| operand.is_a?(Integer) }

    raise UnsupportedOperation unless OPS.member?(operator)
  end

  def to_s
    @report || @result
  end
end
