class Feature
  def self.available?(name)
    # All features disabled by default.
    false
  end
end

p Feature.available?('user_prefs')
# => false

#
# Possible to add new methods, or to override a method to do something
# else.
#

# A form of stubbing a method. Produces warnings with `ruby -w` as
# we are redefining an existing method.
def Feature.available?(name)
  name == 'theme_light'
end

p Feature.available?('edit')
#=> false

p Feature.available?('theme_light')
#=> true
