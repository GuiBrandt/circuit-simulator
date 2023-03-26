# MO601/MC973 — Projeto 1: Um simulador super básico de circuitos lógicos

https://www.ic.unicamp.br/~rodolfo/mo601/projeto1/

## Execução

Na pasta raíz do projeto:

```shell
docker build -t mo601-p1 .
docker run -v ${PWD}/test:/test mo601-p1 -- "/test/*"
```
