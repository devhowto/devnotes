clean:
	rm -rv ./build/{dbsql,editors}

build:
	ruby ./adocwiki/main.rb
