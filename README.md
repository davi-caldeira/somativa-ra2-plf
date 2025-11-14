# Sistema de Inventário em Haskell

## 1. Identificação do Projeto
**Instituição:** Pontifícia Universidade Católica do Paraná <br>
**Disciplina:** Programação Lógica e Funcional <br>
**Professor:** Frank Alcantara <br>
**Atividade:** RA2 – Sistema de Inventário em Haskell <br>

**Repositório GitHub:**  
https://github.com/davi-caldeira/somativa-ra2-plf  

**Execução online (Replit):**  
https://replit.com/@KingALT/somativa-ra2-plf?v=1  

---

## 2. Equipe e Responsabilidades

| Aluno                               | GitHub              | Responsabilidade Principal                                          |
| ----------------------------------- | ------------------- | -------------------------------------------------------------------- |
| **Hiago Bernardo da Silva Rigon**   | `MISTICxxx`         | Definição de tipos e lógica de negócio (`InventoryData.hs`, `InventoryLogic.hs`) |
| **Davi Marques Caldeira**           | `davi-caldeira`     | Módulo principal, I/O, loop de comandos e persistência (`Main.hs`)  |
| **João Victor Balvedi**             | `JoaoVictorBalvedi` | Análise de logs e geração de relatórios (`InventoryReport.hs`)      |

---

## 3. Objetivo do Projeto

O objetivo deste trabalho é desenvolver um **sistema de inventário** em Haskell, aplicando conceitos de **programação funcional**, em especial:

- Separação clara entre **lógica pura** e **operações de I/O**.  
- Uso de tipos imutáveis e estruturas como `Data.Map` para representar o inventário.  
- Tratamento de erros de negócio por meio do tipo `Either`, sem efeitos colaterais na lógica pura.  
- Persistência de dados em arquivos, permitindo que o inventário seja recuperado entre execuções.  
- Registro de todas as operações em um **log de auditoria** para posterior análise e geração de relatórios.

O sistema é interativo via terminal: o usuário digita comandos, o programa processa a entrada, atualiza o estado (quando aplicável) e registra todas as ações em log.

---

## 4. Como Executar o Projeto

### 4.1 Execução Online (Replit)

Para executar o projeto diretamente em um ambiente online:

1. Acesse o Replit do projeto:  
   https://replit.com/@KingALT/somativa-ra2-plf?v=1  
2. Aguarde o carregamento do ambiente.  
3. Clone o projeto.  
4. Use o console do Replit para digitar os comandos do sistema (descritos na seção 5).

---

## 5. Uso do Programa (Comandos)

Após a inicialização, o programa fica em um loop lendo comandos digitados pelo usuário. Os comandos disponíveis são:

### 5.1 `add <id> <nome> <quantidade> <categoria>`

Adiciona um novo item ao inventário.

* `id`: identificador único do item (sem espaços).
* `nome`: nome do item (sem espaços).
* `quantidade`: quantidade inicial em estoque (inteiro).
* `categoria`: categoria do item (sem espaços).

Exemplo:

```text
add item1 Teclado 5 Perifericos
```

Adiciona o item de ID `item1`, nome `Teclado`, quantidade `5`, categoria `Perifericos`.

---

### 5.2 `remove <id> <quantidade>`

Remove uma quantidade de um item existente.

* Se a quantidade solicitada for maior do que o estoque atual, a operação falha e o estoque não é alterado.

Exemplo:

```text
remove item1 3
```

Tenta remover `3` unidades do item `item1`. Se houver estoque suficiente, o inventário é atualizado; caso contrário, uma mensagem de erro é exibida e o log registra a falha.

---

### 5.3 `update <id> <novoNome> <novaCategoria> <novaQuantidade>`

Atualiza os dados de um item já existente:

* Permite alterar `nome`, `categoria` e redefinir a `quantidade` para um novo valor (substitui o valor anterior).

Exemplo:

```text
update item1 TecladoGaming Perifericos 10
```

Atualiza o item `item1` para nome `TecladoGaming`, categoria `Perifericos` e quantidade `10`.

---

### 5.4 `listar`

Lista todos os itens do inventário, exibindo:

* ID
* Nome
* Quantidade
* Categoria

Exemplo:

```text
listar
```

---

### 5.5 `report`

Gera um relatório a partir do arquivo de log (`Auditoria.log`), contendo:

* Todas as operações que falharam (com motivo).
* O item mais movimentado (maior soma de quantidades adicionadas/removidas).
* O total de operações realizadas por item.

Exemplo:

```text
report
```

---

### 5.6 `sair` ou `exit`

Encerra o programa. Antes de sair:

* O inventário atual é salvo em `Inventario.dat`.
* As operações realizadas já estão registradas em `Auditoria.log`.

Exemplo:

```text
sair
```

---

### Observações sobre comandos

* Os identificadores de item (`id`) não diferenciam maiúsculas/minúsculas.
* Recomenda-se evitar espaços em `nome` e `categoria` (usar, por exemplo, `Teclado_Sem_Fio`).
* Comandos mal formatados ou desconhecidos não alteram o inventário e resultam em mensagens de erro no terminal, além de um registro apropriado no log.

---

## 6. Persistência de Dados e Log de Auditoria

O sistema persiste o estado do inventário e registra todas as operações em arquivos em disco:

### 6.1 Arquivo `Inventario.dat`

* Armazena o estado atual do inventário (lista/mapa de itens).
* É sobrescrito a cada operação bem-sucedida que altera o inventário (`add`, `remove`, `update`).
* Na inicialização, o programa tenta ler `Inventario.dat`:

  * Se o arquivo existir e estiver válido, o inventário é restaurado.
  * Se o arquivo não existir (primeira execução), o inventário começa vazio.

### 6.2 Arquivo `Auditoria.log`

* Todas as operações solicitadas pelo usuário são registradas, inclusive as que falham.
* Cada entrada de log (`LogEntry`) contém:

  * Timestamp (data/hora da operação).
  * Ação (`Add`, `Remove`, `Update`).
  * Dados relacionados ao item/quantidade.
  * Status (`Sucesso` ou `Falha` com mensagem explicativa).
* O arquivo é aberto em modo append, preservando o histórico de execuções anteriores.

### 6.3 Separação entre Lógica Pura e I/O

* A lógica de negócio (como adicionar, remover ou atualizar itens) é implementada em funções puras, que:

  * Recebem o inventário atual e parâmetros da operação.
  * Retornam um `Either`:

    * `Right (novoInventario, logDaOperacao)` em caso de sucesso.
    * `Left mensagemDeErro` em caso de falha (por exemplo, estoque insuficiente).
* As funções puras não acessam arquivos nem o terminal.
* O módulo `Main` é responsável por:

  * Ler e gravar `Inventario.dat` e `Auditoria.log`.
  * Obter o timestamp.
  * Exibir mensagens ao usuário.
  * Chamar as funções puras e, com base no resultado, decidir o que persistir.

Esse desenho reforça o paradigma funcional, isolando efeitos colaterais e mantendo a lógica testável de forma independente.

---

## 7. Estrutura do Código-Fonte

Organização dos arquivos do projeto:

```text
somativa-ra2-plf/
├── InventoryData.hs      -- Definição de tipos: Item, Inventario, AcaoLog, StatusLog, LogEntry, etc.
├── InventoryLogic.hs     -- Funções puras de negócio: addItem, removeItem, updateItem (retornam Either)
├── InventoryReport.hs    -- Funções puras para análise de log: logsDeErro, historicoPorItem, itemMaisMovimentado
├── Main.hs               -- Função main, loop de comandos, interação com usuário e I/O (arquivos, tempo)
└── README.md             -- Documentação do projeto
```

* `InventoryData.hs`
  Define os tipos de dados utilizados em todo o sistema e deriva instâncias de `Show` e `Read`, permitindo serialização simples com `show`/`read`.

* `InventoryLogic.hs`
  Implementa a lógica do inventário de forma puramente funcional, usando `Either` para representar sucesso ou falha, sem executar qualquer I/O.

* `InventoryReport.hs`
  Processa listas de `LogEntry` e extrai informações para o relatório: erros, item mais movimentado e contagem de operações por item.

* `Main.hs`
  Controla o fluxo do programa: lê comandos, chama a lógica apropriada, faz a persistência em disco e encerra quando solicitado.

---

## 8. Cenários de Teste Manual

A seguir, alguns cenários de testes manuais realizados para validar o comportamento do sistema.

### 8.1 Cenário 1 – Persistência de Estado

Objetivo: verificar se o inventário é corretamente salvo e restaurado.

1. Executar o programa em um diretório sem `Inventario.dat` nem `Auditoria.log`.

2. Rodar os comandos:

   ```text
   add item1 Mouse 10 Perifericos
   add item2 Teclado 5 Perifericos
   add item3 Monitor 2 Video
   ```

3. Digitar `sair` para encerrar o programa.

4. Confirmar que foram criados:

   * `Inventario.dat` contendo os 3 itens.
   * `Auditoria.log` contendo 3 operações com status de sucesso.

5. Executar o programa novamente.

6. Digitar `listar` e verificar se os 3 itens aparecem com os mesmos dados.

Resultado esperado:

* O inventário é restaurado com os 3 itens.
* O log contém as operações realizadas.
* Não ocorrem erros de leitura.

---

### 8.2 Cenário 2 – Estoque Insuficiente

Objetivo: garantir que o sistema não permite remoções acima do estoque disponível e registra o erro.

1. Com o inventário já contendo `item2` com quantidade `5` (por exemplo, após o Cenário 1), executar:

   ```text
   remove item2 15
   ```

2. Observar a mensagem exibida no terminal (erro de estoque insuficiente).

3. Executar `listar` e verificar que a quantidade de `item2` continua `5`.

4. Conferir em `Auditoria.log` a presença de uma entrada de falha para essa operação.

Resultado esperado:

* A operação não altera o inventário.
* O usuário recebe mensagem de erro clara.
* A tentativa é registrada em `Auditoria.log` como falha por estoque insuficiente.

---

### 8.3 Cenário 3 – Geração de Relatório

Objetivo: validar o comando `report` e as informações geradas a partir do log.

1. Após realizar operações bem-sucedidas e pelo menos uma falha (como no Cenário 2), executar:

   ```text
   report
   ```

2. Verificar se o relatório exibe:

   * Lista das operações que falharam (por exemplo, a remoção inválida do Cenário 2).
   * Item mais movimentado (no exemplo, `item1 Mouse` com 10 unidades adicionadas).
   * Quantidade de operações por item (`item1`, `item2`, `item3`, etc.).

Resultado esperado:

* Os dados do relatório condizem com o histórico das operações realizadas.
* O item mais movimentado e as contagens por item refletem as ações de `add`, `remove` e `update` executadas pelo usuário.