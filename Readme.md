## Installation Instructions
- Get neo4j desktop. https://neo4j.com/download/
- Clone our github repository (it's a private repository). https://github.com/chscholl/GraphRedex
- Run neo4j desktop, then:
	- Create a project
	- Create a graph(=database) with password "neo4j-js-password" (this password is hardcoded in client.js)
	- Start it
	- Click "Manage", then "Open Browser"
- `./installRequiredRacketPackages.sh`
	to install the required racket packages
- `racket RedexServer/Threads.rkt`
	to run the racket server
- Open WebClient/index.html in your browser
	- Click "Send term"
- Click the blue triangle button to show the graph in the webpage
- Click on any graph node to have its term written in the text area
- Click "Send term" again...

## Credits
- Cy2Neo: https://github.com/jexp/cy2neo