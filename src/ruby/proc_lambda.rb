# rubocop:disable all

#
# • https://gist.github.com/ParadoxV5/b2eeee77aa9c9601e4a0ea7cd26b9090
# • https://discord.com/channels/518658712081268738/650031651845308419/1102819723890139167
#
# Proc and lambda share the same class {Proc}.
# {Proc#lambda?} queries if a Proc is a lambda or not.
#
# You can create a proc with {Proc.new} or {Kernel#proc}.
# You can create a lambda with the `->(){}` syntax or {Kernel#lambda}.
#
# `&blk` converts block `blk` to proc (when receiving args)
# or procs or lambda to block (when passing args).
#
# Invoke a proc or lambda with {Proc#call}
# or one of the aliases: {Proc#[]}, {Proc#yield}, {Proc#===}.
# `.()` is syntax-sugar for `.call`.
#

require 'rspec'

RSpec.describe 'Proc vs. Lambda' do
  describe 'Implementation' do
    p =   proc { }
    l = lambda { }

    it 'use class `Proc` for both procs and lambdas' do
      expect(p).to be_an_instance_of Proc
      expect(l).to be_an_instance_of Proc
    end

    it 'distinguishes lambdas and non-lambdas with `Proc#lambda?`' do
      expect(l).to be_a_lambda
      expect(p).not_to be_a_lambda
    end

    it 'uses the scope they were created in' do
      def make(sym, output) = send(sym) { output }
      a = rand
      expect(make(:proc , -a).()).to equal(-a)
      expect(make(:lambda, a).()).to equal( a)
    end

    it 'checks equality by code block *only*' do
      a = [proc { 'baz' }, lambda { 'baz' }]

      a.product([proc { 'baz' }, lambda { 'baz' }]) do |outer, inner|
        expect(inner).to_not eql outer
      end

      a.product([:proc, :lambda]) do |blk, meth|
        expect(send(meth, &blk)).to eql blk
      end
    end
  end

  describe 'Similarities' do
    it 'supports blocks with optional args' do
      a, v = rand, rand
      p = proc   {|x = a| x }
      l = lambda {|x = a| x }
      expect(p.( )).to equal a
      expect(l.( )).to equal a
      expect(p.(v)).to equal v
      expect(l.(v)).to equal v
    end

    it 'supports blocks with ignored args' do
      v = rand
      p = proc   {|_| v }
      l = lambda {|_| v }
      expect(p.('foo')).to equal v
      expect(l.('bar')).to equal v
    end

    it 'does not auto-splat an 1-tuple array actual arg for a singular formal arg' do
      a, v = rand.to_s, rand
      p = proc   {|x = v| x.reverse }
      l = lambda {|x = v| x.reverse }
      expect(p.([a])).to eql [a]
      expect(l.([a])).to eql [a]
    end

    it '`next` returns the block' do
      a, b, n = rand, rand, rand(1..5)
      expect(n.times.map       { next b } ).to eql Array.new(n, b)
      expect(n.times.map(&->(_){ next a })).to eql Array.new(n, a)
    end
  end

  describe 'Differences' do
    def rng
      f = rand
      3.times.map { _1 + f }
    end

    it 'only checks lambda arg count' do
      a, b, c = rng
      p = proc   {|x, y| [x, y] }
      l = lambda {|x, y| [x, y] }
      expect { l.(c) }.to raise_error ArgumentError
      expect(p.(c)).to eql [c, nil]
      expect { l.(a, b, c) }.to raise_error ArgumentError
      expect(p.(a, b, c)).to eql [a, b]
    end

    it 'auto-splats an 1-tuple array actual arg for a proc with multiple formal args' do
      *a, v = rng
      p = proc   {|x = v, y = v| [x, y] }
      l = lambda {|x = v, y = v| [x, y] }
      expect(p.(a)).to eql a
      expect(l.(a)).to eql [a, v]
    end

    # Did ParadoxV5 mean “stabby” instead of “stubby”?
    describe 'NOTE: Use blocks and “stubby” lambdas, otherwise the blocks are really attached to `Kernel#proc` or `Kernel#lambda`' do
      it '`return` returns the current method for block but returns the block for lambda' do
        a, *b = rng
        def f = yield
        def p(x, y) = [ # Fix “undefined local variable or method `v'”
          f { return x }, # returns this
          y
        ]
        def l(x, y) = [
          f(&->{ return x }),
          y # returns this
        ]
        expect(p(a, 0)).to equal a
        expect(l( *b )).to eql b
      end

      it '`break` s breaks out of the passed-to method for block but returns the block for lambda' do
        a, b = rand, rand
        expect(69.times.map        { break b }).to equal b

        n = rand(1..5)
        expect(n.times.map(&->(_){ break a })).to eql Array.new(n, a)
      end
    end
  end

  it 'differences are ignored if the proc and lambda share the same block' do
    a = rand
    expect(lambda(&proc   {|x, y| [x, y] }).(a)).to eql [a, nil] # does not raise ArgumentError
    expect{  proc(&lambda {|x, y| [x, y] }).(a)}.to raise_error ArgumentError
  end
end
