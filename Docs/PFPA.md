# Weathell

*Weathell, the weather API written in Haskell*

A API contém informações de previsão do tempo de regiões com condições como vento, chuva e umidade. Também fornece a fonte responsável pelas previsões.

## Principais funcionalidades

- Previsão do tempo com informações completas com dados da região e das condições de vento, chuva e umidade com nome do meteorologista e organização responsáveis. ``region-conditions/id_regiao``

- Todas as previsões do tempo de uma cidade. ``/region/search/Búzios``

- Previsão de cada condição e fonte separadamente por região ``/wind/id_regiao`` ou ``/source/id_regiao``

- Saber onde vai chover (previsão com probabilidade de chuva maior de 50%) ``/rain/getyourumbrella``

## Rotas

### Região *Region*
<br>
- `` GET`` /region
<br>
Retorna todas as regiões com id, cidade, estado, país, data da previsão, temperatura mínima e máxima

*Request*

``curl -X GET localhost:8080/region -H "accept: application/json"``

*Response*
```
{
   "result":[
      {
         "min":15,
         "id":5,
         "max":30,
         "state":"RJ",
         "country":"Brasil",
         "date":"2022-03-01",
         "city":"Búzios"
      },
      {
         "min":15,
         "id":6,
         "max":30,
         "state":"RJ",
         "country":"Brasil",
         "date":"2022-03-01",
         "city":"Rio de Janeiro"
      },
      {
         "min":15,
         "id":7,
         "max":30,
         "state":"RJ",
         "country":"Brasil",
         "date":"2022-03-05",
         "city":"Rio de Janeiro"
      }
   ]
}
```
<br><br>
- `` POST`` /region
<br>
Insere uma nova região com todos os campos obrigatórios e retorna o ID

*Request*

``curl -X POST localhost:8080/region -d '{"city": "Rio de Janeiro", "state": "RJ", "country": "Brasil", "date": "2022-03-02", "min": 19, "max": 30}' -H "accept: application/json" -H "Content-Type: application/json"``

*Response*
```
{
   "result":8
}
```
<br><br>
- `` GET`` /region/#RegionId
<br>
Retorna uma região com ID, cidade, estado, país, data da previsão, temperatura mínima e máxima

*Request*

``curl -X GET localhost:8080/region/3 -H "accept: application/json"``

*Response*
```
{
   "result":{
      "min":15,
      "max":30,
      "state":"RJ",
      "country":"Brasil",
      "date":"2022-02-28",
      "city":"Búzios"
   }
}
```
<br><br>
- `` PATCH`` /region/#RegionId
<br>
Edita cidade, estado ou país (somente um campo é obrigatório) e não possui retorno

*Request*

``curl -X PATCH localhost:8080/region/3 -d '{"regionMaybeCity": "Iguaba"}' -H "accept: application/json" -H "Content-Type: application/json"``
<br><br>
- `` DELETE`` /region/#RegionId
<br>
Deleta uma região e não possui retorno

*Request*

``curl -X DELETE localhost:8080/region/3 -H "accept: application/json"``
<br><br>
- `` GET`` /region/search/#Text
<br>
Busca uma região por nome da cidade e retorna todos os campos da região. A requisição deve respeitar letras maiúsculas, minúsculas e acentos. Ex.: Rio de Janeiro, São Paulo, São Pedro da Aldeia.

*Request*

``curl -X GET localhost:8080/region/search/São%20Paulo -H "accept: application/json"``

*Response*
```
{
   "result":[
      {
         "min":8,
         "id":1,
         "max":25,
         "state":"SP",
         "country":"Brasil",
         "date":"2022-02-26",
         "city":"São Paulo"
      }
   ]
}
```
<br><br>
- `` GET`` /region-conditions/#RegionId
<br>
Retorna uma região e todas as suas condições (vento, chuva, umidade e fonte)

*Request*

``curl -X GET localhost:8080/region-conditions/3 -H "accept: application/json"``

*Response*
```
{
   "result":{
      "wind":[
         {
            "regionId":3,
            "afternoon":10,
            "id":3,
            "overnight":26,
            "evening":20,
            "morning":20
         }
      ],
      "region":{
         "min":15,
         "max":30,
         "state":"RJ",
         "country":"Brasil",
         "date":"2022-02-28",
         "city":"Búzios"
      },
      "rain":[

      ],
      "humidity":[
         {
            "regionId":3,
            "min":1,
            "id":1,
            "max":12
         }
      ],
      "source":[
         {
            "regionId":3,
            "id":4,
            "organization":"USP",
            "weatherperson":"Claudia"
         }
      ]
   }
}
```

<br>

### Fonte *Source*
<br>
- `` GET`` /source
<br>
Retorna todas as fontes com nome do meteorologista e organização responsáveis pela previsão de cada região (identificadas por ID)

*Request*

``curl -X GET localhost:8080/source -H "accept: application/json"``

*Response*
```
{
   "result":[
      {
         "regionId":2,
         "id":2,
         "organization":"INMET",
         "weatherperson":"Andrew"
      },
      {
         "regionId":3,
         "id":4,
         "organization":"USP",
         "weatherperson":"Claudia"
      }
   ]
}
```
<br><br>
- `` POST`` /source
<br>
Insere uma nova fonte com todos os campos obrigatórios e retorna o ID. Cada região só permite uma fonte por dia de previsão

*Request*

``curl -X POST localhost:8080/source -d '{"regionId": 3,"weatherperson": "Claudia", "organization": "USP"}' -H "accept: application/json" -H "Content-Type: application/json"``

*Response*
```
{
   "result":4
}
```
<br><br>
- `` GET`` /source/#RegionId
<br>
Retorna uma fonte de acordo com a sua região com ID, ID da região, nome do meteorologista e organização responsáveis pela previsão

*Request*

``curl -X GET localhost:8080/source/3 -H "accept: application/json"``

*Response*
```
{
   "result":[
      {
         "regionId":3,
         "id":4,
         "organization":"USP",
         "weatherperson":"Claudia"
      }
   ]
}
```
<br><br>
- `` PATCH`` /source/edit/#SourceId
<br>
Edita nome do meteorologista e organização (somente um campo é obrigatório) e não possui retorno

*Request*

``curl -X PATCH localhost:8080/source/edit/3 -d '{"sourceMaybeWeatherperson": "Rosa"}' -H "accept: application/json" -H "Content-Type: application/json"``
<br><br>
- `` DELETE`` /source/edit/#SourceId
<br>
Deleta uma fonte e não possui retorno

*Request*

``curl -X DELETE localhost:8080/source/edit/1 -H "accept: application/json"``

<br>

### Vento *Wind*
<br>
- `` GET`` /wind
<br>
Retorna todas as informações de previsão de vento em quatro períodos do dia de cada região (identificadas por ID)

*Request*

``curl -X GET localhost:8080/wind -H "accept: application/json"``

*Response*
```
{
   "result":[
      {
         "regionId":3,
         "afternoon":10,
         "id":3,
         "overnight":26,
         "evening":20,
         "morning":20
      },
      {
         "regionId":7,
         "afternoon":10,
         "id":7,
         "overnight":12,
         "evening":60,
         "morning":10
      }
   ]
}
```
<br><br>
- `` POST`` /wind
<br>
Insere uma informação sobre vento com todos os campos obrigatórios e retorna o ID. Cada região só permite uma informação por dia de previsão

*Request*

``curl -X POST localhost:8080/wind -d '{"regionId": 7,"morning": 10, "afternoon": 10, "evening": 60, "overnight": 12}' -H "accept: application/json" -H "Content-Type: application/json"``

*Response*
```
{
   "result":8
}
```
<br><br>
- `` GET`` /wind/#RegionId
<br>
Retorna uma informação sobre vento de acordo com a sua região com ID, ID da região e previsão de vento em quatro períodos do dia

*Request*

``curl -X GET localhost:8080/wind/7 -H "accept: application/json"``

*Response*
```
{
   "result":[
      {
         "regionId":7,
         "afternoon":10,
         "id":8,
         "overnight":12,
         "evening":60,
         "morning":10
      }
   ]
}
```
<br><br>
- `` PUT`` /wind/edit/#WindId
<br>
Edita todos os campo, que são obrigatórios, e não possui retorno

*Request*

``curl -X PUT localhost:8080/wind/edit/2 -d '{"regionId": 2,"morning": 40, "afternoon": 15, "evening": 40, "overnight": 6}' -H "accept: application/json" -H "Content-Type: application/json"``

- `` DELETE`` /wind/edit/#WindId
Deleta uma informação sobre vento e não possui retorno

*Request*

``curl -X DELETE localhost:8080/wind/edit/7 -H "accept: application/json""``

<br>

### Chuva *Rain*
<br>
- `` GET`` /rain
<br>
Retorna todas as informações de previsão de chuva com volume e probabilidade de cada região (identificadas por ID)

*Request*

``curl -X GET localhost:8080/rain -H "accept: application/json"``

*Response*
```
{
   "result":[
      {
         "regionId":2,
         "id":3,
         "probability":30,
         "volume":3
      },
      {
         "regionId":5,
         "id":4,
         "probability":10,
         "volume":3
      },
      {
         "regionId":6,
         "id":10,
         "probability":10,
         "volume":3
      },
      {
         "regionId":7,
         "id":12,
         "probability":60,
         "volume":3
      }
   ]
}
```
<br><br>
- `` POST`` /rain
<br>
Insere uma informação sobre chuva com todos os campos obrigatórios e retorna o ID. Cada região só permite uma informação por dia de previsão

*Request*

``curl -X POST localhost:8080/rain -d '{"regionId":7, "volume": 5, "probability": 70}' -H "accept: application/json" -H "Content-Type: application/json"``

*Response*
```
{
   "result":16
}
```
<br><br>
- `` GET`` /rain/#RegionId
<br>
Retorna uma informação sobre chuva de acordo com a sua região com ID, ID da região, volume e probabilidade do dia

*Request*

``curl -X GET localhost:8080/rain/3 -H "accept: application/json"``

*Response*
```
{
   "result":[
      {
         "regionId":3,
         "id":16,
         "probability":70,
         "volume":5
      }
   ]
}
```
<br><br>
- `` PUT`` /rain/edit/#RainId
<br>
Edita todos os campo, que são obrigatórios, e não possui retorno

*Request*

``curl -X PUT localhost:8080/rain/edit/1 -d '{"regionId": 3, "volume": 1, "probability": 12}' -H "accept: application/json" -H "Content-Type: application/json"``
<br><br>
- `` DELETE`` /rain/edit/#RainId
<br>
Deleta uma informação sobre chuva e não possui retorno

*Request*

``curl -X DELETE localhost:8080/rain/edit/3 -H "accept: application/json""``
<br><br>
- `` GET`` /rain/getyourumbrella
<br>
Retorna informações de onde deve chover (regiões com probabilidade maior de 50%) com ID, ID da região, volume e probabilidade do dia

*Request*

``curl -X GET localhost:8080/rain/getyourumbrella -H "accept: application/json"``

*Response*
```
{
   "result":[
      {
         "regionId":7,
         "id":12,
         "probability":60,
         "volume":3
      },
      {
         "regionId":3,
         "id":16,
         "probability":55,
         "volume":1
      }
   ]
}
```

<br>

### Umidade *Humidity*
<br>
- `` GET`` /humidity
<br>
Retorna todas as informações de previsão de umidade com máxima e mínima de cada região (identificadas por ID)

*Request*

``curl -X GET localhost:8080/humidity -H "accept: application/json"``

*Response*
```
{
   "result":[
      {
         "regionId":3,
         "min":1,
         "id":1,
         "max":12
      },
      {
         "regionId":7,
         "min":10,
         "id":4,
         "max":20
      }
   ]
}
```
<br><br>
- `` POST`` /humidity
<br>
Insere uma informação sobre umidade com todos os campos obrigatórios e retorna o ID. Cada região só permite uma informação por dia de previsão

*Request*

``curl -X POST localhost:8080/humidity -d '{"regionId": 3, "min": 10, "max": 20}' -H "accept: application/json" -H "Content-Type: application/json""``

*Response*
```
{
   "result":4
}
```
<br><br>
- `` GET`` /humidity/#RegionId
<br>
Retorna uma informação sobre umidade de acordo com a sua região com ID, ID da região, máxima e mínima do dia

*Request*

``curl -X GET localhost:8080/humidity/3 -H "accept: application/json"``

*Response*
```
{
   "result":[
      {
         "regionId":3,
         "min":1,
         "id":1,
         "max":12
      }
   ]
}
```
<br><br>
- `` PUT`` /humidity/edit/#HumidityId
<br>
Edita todos os campo, que são obrigatórios, e não possui retorno

*Request*

``curl -X PUT localhost:8080/humidity/edit/1 -d '{"regionId": 3, "min": 5, "max": 9}' -H "accept: application/json" -H "Content-Type: application/json"``
<br><br>
- `` DELETE`` /humidity/edit/#HumidityId
<br>
Deleta uma informação sobre umidade e não possui retorno

*Request*

``curl -X DELETE localhost:8080/humidity/edit/3 -H "accept: application/json""``

<br>

## Créditos

Autora: Clarissa R. Mendes

Data: 27 de fevereiro de 2022

Projeto do curso de verão Programação Funcional Pura com Aplicações/ IME-USP