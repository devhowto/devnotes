##
# Improvements made from code review and mentoring from the @kotp.
##

module CalculatorExceptions
  class UnsupportedOperationError < ArgumentError
    def initialize(message = 'A valid operation must be provided.')
      super
    end
  end

  UnsupportedOperation = UnsupportedOperationError
end

module SimpleCalculatorExceptions
  class IntegerOperandError < ArgumentError
    def initialize(message = 'Operands must be integers.')
      super
    end
  end
end

class SimpleCalculator
  include CalculatorExceptions, SimpleCalculatorExceptions

  OPERATE = {
    '+' => ->(operand1, operand2) { operand1 + operand2 },
    '*' => ->(operand1, operand2) { operand1 * operand2 },
    '/' => ->(dividend, divisor) { dividend / divisor },
  }

  REPORT = '%<operand1>i %<operator>s %<operand2>i = %<result>i'

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
    if operator == '/' and operand2.zero?
      return 'Division by zero is not allowed.'
    end

    raise UnsupportedOperation unless operation_allowed?(operator) &&
      [operand1, operand2].all? { |operand| operand.is_a?(Integer) }

    new(operand1, operand2, operator).to_s
  end

  private

  attr_reader :report

  def self.operation_allowed?(operator)
    OPERATE.keys.member?(operator)
  end

  def initialize(operand1, operand2, operator, report: REPORT)
    @operand1 = operand1
    @operand2 = operand2
    @operator = operator
    @report = report
  end

  def operate
    OPERATE[operator].call(operand1, operand2)
  end

  public

  attr_reader :operand1, :operand2, :operator, :report

  def to_s
    report % {
      operand1:,
      operator:,
      operand2:,
      result: operate
    }
  end
end

if $PROGRAM_NAME == __FILE__
  puts SimpleCalculator.calculate(1, 2, '+')
  puts SimpleCalculator.new(1, 2, '+')
  p SimpleCalculator.new(1, 2, '+').to_s

  puts
  my_report = <<~eos
    %<operand1>4i
    ———— = %<result>s
    %<operand2>4i
  eos

  puts SimpleCalculator.new(1024, 4, '/', report: my_report)

  puts

  class CustomSimpleCalculator < SimpleCalculator
    if OPERATE['**'].nil?
      OPERATE['**'] = ->(base, power) { base ** power }
    end
  end

  puts CustomSimpleCalculator.calculate(2, 8, '**')
end
