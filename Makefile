prod: src/*.elm
	elm make src/Main.elm --output=app.js --optimize

# launch live reloading server for development
live:
	elm-live src/Main.elm --start-page=index.html --open -- --debug --output=app.js

