require_relative 'file_retriever'

class Job
  ##
  # BAG DESIGN
  #
  # Job#run has to know about two classes.
  # For tests, we would probably stub them.
  #
  # Pretend get_file returns "hello%20world" content, and we want
  # to replace %20 with a space (' ')
  #
  def run
    @retriever = FileRetriever.new
    content = @retriever.get_file('theirs.txt')

    @cleaner = FileCleaner.new
    cleaned = @cleaner.clean(content)

    File.open('./mycopy.txt', 'w') { |f| f.write(cleaned) }

    cleaned
  end
end
