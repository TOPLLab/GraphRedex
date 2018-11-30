# Installation on OSX 

### Installing Arangodb 

Graphredex uses a graph database called arangodb. 
The easiest way to install this database is by using brew.

```
brew install arangodb
/usr/local/opt/arangodb/sbin/arangod &
```

### Configuration of Arangodb 
Once the databse is installed it needs tobe initialised with an initial database and two users. 
Configuration of the datbase is broswer based, the default address is http://127.0.0.1:8529. 


### Creating users 

In order for GraphRedex to work it needs to have two users in the system called 'graphredex-qry' and 'graphredex'.
The user graphredex has read and write access while the user graphredex-qry only has read acces. 

- In the browser, go to the tab "Users", and create a user called 'graphredex-qry' with password 'graphredex-qry'. 
- In the browser, go to the tab "Users", and create a user called 'graphredex' with password 'graphredex'.

#### Creating the database  

Go to the tab "Databases" and make a database called 'graphredex-test' make sure to select graphredex as the owner. 

#### Setting the permissions 

Now that the database is created we still need to make sure that the permissions of the users are correct:

Go to the Users tab and select permissions tab: In Users/graphredex in de permissions tab:
- geeft graphredex administrative acces op de databank graphredex-test 
- geeft graphredex read/write acces op alle collections (de rij met *) 

- In Users/graphredex-qry in de permissions tab:
* geeft graphredex-qry acces acces op de databank graphredex-test 
* geeft graphredex-qrt read acces op alle collections (de rij met *) 

# Installation of GraphRedex

- Clone the repository 
- Go to the folder APIServer  

## Install the packaging tools 

```
brew install yarn
```
## 

Execute 
```
yarn install
``` 

Execute 
```
./start.sh -c -b
```

## Known issues when uploading a new language 

```
(require redex)
(require redex/reduction-semantics)
```
