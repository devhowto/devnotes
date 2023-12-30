require_relative 'job'
require_relative 'file_retriever'
require_relative 'file_cleaner'

describe Job do
  it 'should retrieve file and store it locally' do
    #
    # Because of our bad design, we have to mock a bunch of classes.
    # Even though we are not doing it here, we should probably also
    # mock File.open.
    #

    file_retriever = double('file_retriever')
    allow_any_instance_of(
      FileRetriever
    ).to receive(:get_file).and_return('the%20force')

    allow_any_instance_of(
      FileCleaner
    ).to receive(:clean).and_return('the force')

    job = Job.new

    expect(job.run).to eq('the force')
    expect(File.read('./mycopy.txt')).to match(/the force/)

    FileUtils.rm('./mycopy.txt')
  end
end
