require_relative 'file_retriever'

class Job
  attr_reader :local_path

  ##
  # The class now takes the dependencies from parameters.
  #
  # We only care that the retriever has a get_file method
  # and the cleaner has a clean method.
  #
  def initialize(
    retriever,
    cleaner,
    remote_path,
    local_path
  )
    @retriever = retriever
    @cleaner = cleaner
    @remote_path = remote_path
    @local_path = local_path
  end

  ##
  # See notes on the tests. This method seems to be doing too much.
  #
  def run
    content = @retriever.get_file(@remote_path)

    @cleaner = FileCleaner.new
    cleaned = @cleaner.clean(content)

    File.open(@local_path, 'w') { |f| f.write(cleaned) }

    cleaned
  end
end
