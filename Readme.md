## Setup
- Get neo4j desktop. https://neo4j.com/download/
- Clone our github repository (it's a private repository). https://github.com/chscholl/GraphRedex
- Run neo4j desktop, then:
	- Create a project
	- Create a graph(=database) with password "neo4j-js-password" (this password is hardcoded in client.js)
- Run `./installRequiredRacketPackages.sh`
	to install the required racket packages for the reduction server

## Running GraphRedex
- Run neo4j desktop
	- "Start" the graph created during the setup
- Run one of the racket files found in the RedexServer folder to run the reduction server. For example: `racket RedexServer/Threads.rkt`. Each file corresponds to a different language and contains the grammar of its terms and its reduction rules.
- Open WebClient/index.html in your browser

## Tutorial

**Note:** *If you do not send any term to the reduction server for some time, the connection between it and the web client will close, preventing you from sending other terms. When this happens, restart the reduction server and refresh the web client webpage to reconnect.*

#### Tutorial 1: Sending a term and reducing it step-by-step manually
- Run `racket RedexServer/Threads.rkt` as reduction server.
- In the web client:
	- Click on "Sample Program 1 (Threads)" or type a term yourself in the text box (the specification of the "Threads" language is in RedexServer/Threads.rkt).
	- Click on "Use as origin for new program (1 reduction steps)" to send the term to the reduction server and have the resulting reduction graph displayed in the web client.
	- You should see an orange node (representing the term you sent) pointing to two other grey nodes (representing the two possible one-step reductions of the term you sent)
	- Left-click a grey node to open the contextual menu, and click "Reduce Once And Refresh" to send the corresponding term to the reduction server and continue the reduction graph.

#### Tutorial 2: Reducing a term 50 steps in one go
- Run `racket RedexServer/Threads.rkt` as reduction server.
- In the web client:
	- Click "Clear Database" to clear the reduction graph obtained at the previous steps.
	- Click on "Sample Program 1 (Threads)".
	- Click on "Use as origin for new program (50 reduction steps)". This button makes the web client recursively send back to the reduction server the reduced terms obtained from it, up to 50 times.

-> You should see the entire reduction graph directly, because the example program is small and takes less than 50 steps to reduce entirely.

#### Tutorial 3: Using a different language
- In the web client:
	- Click "Clear Database" to clear the reduction graph obtained at the previous steps.
- Stop the reduction server
- Run a reduction server for a different language by running a different .rkt file from the RedexServer folder. For example `racket RedexServer/Threadslock.rkt`
- Refresh the web client webpage to connect to the new reduction server

#### Tutorial 4: Using your own language
- Copy one of the .rkt files available in the RedexServer folder and modify it. More specifically, each .rkt files contains three elements you need to modify:
	- the grammar describing the terms of your language
	- the reduction rules of your language
	- an extraction function: a function taking as input a term of your language and outputting a list of key-value couples. The key-value returned by this function will be added as attributes to the nodes representing the terms in the graph database. This allows graph queries to use these extra attributes, and they will also be displayed in the node infobox of the web client (that shows information about the last left-clicked node). These key-values can be any information about your terms that you are interested in.
- Follow tutorial number 3 using the file you created at the above step.

#### Tutorial 5: Performing graph queries
- Run `racket RedexServer/Threadslock.rkt` as reduction server to use the Threadslock language. Refer to Threadslock.rkt for a formal and informal description of the Threadslock language.
- In the web client:
	- Click "Clear Database" to clear any reduction graph obtained at the previous steps.
	- Click on "Sample Program 2 (Threadslock)"
	- Click on "Use as origin for new program (50 reduction steps)"
	- The full reduction graph should start with a diamond of nodes, with two paths out of it that each contain a diamond of nodes and converge to the same final node.
	- Click on "Path to deadlocks query template (sample program 2)". This will write a graph query in the top-most text box. This query is written in the [cypher language](https://neo4j.com/developer/cypher-query-language/) and asks for the paths of arbitrary length going from the origin node to nodes with no outgoing edge and whose value for *x* and *y* are different from 0.
	- Click on "Perform query without graph re-render" to perform the query.
	- This should display an infobox with the results of the query. The query should return two paths, which you can visualise by clicking on the "mark" buttons. Click "Clear markings" to remove all node markings.
	- You can click on the blue triangle in the top-right-hand corner of the screen to perform the query and have the graph viewer replace the reduction graph with the nodes part of the query's result. Note that this feature has not been developed much and can put the UI in an inconsistent state. To see the whole reduction graph again, you can click on "Show full graph query template" and after on the blue triangle again.


## Credits
- Cy2Neo: https://github.com/jexp/cy2neo