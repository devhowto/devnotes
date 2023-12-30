require_relative 'job'
require_relative 'file_retriever'
require_relative 'file_cleaner'

describe Job do
  #
  # Now because the dependencies are passed as parameters we can
  # pass true mock objects as params to inject them.
  #
  it 'should retrieve file and store it locally' do
    retriever = double('FileRetriever')
    allow(retriever).to receive(:get_file).and_return('the%20force')

    cleaner = double('FileCleaner')
    allow(retriever).to receive(:clean).and_return('the force')

    job = Job.new(retriever,
                  cleaner,
                  './some/file.txt',
                  './my_local_copy.txt',
                 )

    expect(job.run).to eq('the force')
    expect(File.read('./my_local_copy.txt')).to match(/the force/)

    FileUtils.rm('./my_local_copy.txt')
  end
end
