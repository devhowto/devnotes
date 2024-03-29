class Attendee
  def initialize(height)
    @height = height
    @pass_id = nil
  end

  def height
    @height
  end

  def pass_id
    @pass_id
  end

  def issue_pass!(pass_id)
    ##
    # Assigns `pass_id` to the instance variable `@pass_id` only if
    # `pass_id` is an integer. The value assigned is then returned
    # because the whole assignment line is an expression, and
    # expressions produce values (unlike statements).
    #
    # If `pass_id` is not an integer, no assignment occurs, and the
    # instance variable `@pass_id` retains its previous value.
    #
    @pass_id = pass_id if pass_id.is_a?(Integer)
  end

  def revoke_pass!
    @pass_id = nil
  end
end
