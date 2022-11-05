# task (Taskwarrior)
## Environment variables
TASK_DATA_DIR

## Run as docker container
```
docker run -it --rm --name task -v ${TASK_DATA_DIR}:/home/user/.task/ ${DOCKER_REPO_PREFIX}/task "task"
```
You might need to run the container as a user that exists on the host, by adding e.g. --user 1026