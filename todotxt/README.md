# todotxt
## Environment variables
TODOTXT_TODO_DIR

## Run as docker container
```
docker run -it --rm --name todotxt -v ${TODOTXT_TODO_DIR}:/home/user/workspace/ ${DOCKER_REPO_PREFIX}/todotxt "todo.sh lsa"
```
You might need to run the container as a user that exists on the host, by adding e.g. --user 1026

The config assumes that your todo, done, report files are located in ~/workspace/todo
