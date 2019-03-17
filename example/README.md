# ocaml-export servant server and reason-react frontend example

## Structure

- shared-types: data types and servant api
- shared-types-re: an executable that generates the Reason types
  from shared-types using ocaml-export
- frontend: a simple Reason React app that uses the automatically
  generated types to query the server
- server: a servant server with a mock database, uses shared-types
  and serves the fronted

## run

The compile process is not automated yet. You have to do it manually for the time being.
Any time you change the types you need update fronted and the server. Anytime you change
the frontend you need to compile it and re-copy the output to the server's static directory.

```
cd shared-types-re
stack build
stack exec generate-reason
cd ../frontend
yarn
yarn build
cd ../server
rm -rf static
cp -rf ../frontend/build static
stack build
stack exec server
```

Now you can view the app in your browser `localhost:8001/index.html`

## Todo

- automate the build process
- add authentication
- add admin interface
- unhardcode a few things
- add comments to the code
