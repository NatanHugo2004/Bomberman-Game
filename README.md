# Bomberman

Bomberman é um jogo simples inspirado no clássico, desenvolvido em Haskell com uma abordagem de programação funcional. O projeto utiliza a ferramenta de build Stack para gerenciar as dependências e a execução do jogo.

## Funcionalidades

* Jogue uma partida de Bomberman em um mapa 2D renderizado no terminal.
* Mova o personagem e plante bombas para destruir caixas.
* Encontre a chave escondida para abrir a porta de saída.
* Vença o jogo escapando pela porta antes que o tempo acabe.

## Pré-requisitos

Certifique-se de ter os seguintes requisitos instalados na sua máquina:

* Haskell (recomendamos utilizar o [GHCup](https://www.haskell.org/ghcup/install/))
* Stack (pode ser instalado com o comando `curl -sSL https://get.haskellstack.org/`)
* Para utilizadores de Windows, é recomendável utilizar o [Subsistema do Windows para Linux (WSL)](https://learn.microsoft.com/pt-br/windows/wsl/install).

## Instalação

1.  Clone o repositório do projeto:
    ```bash
    git clone https://github.com/NatanHugo2004/Bomberman-Game.git
    ```
2.  Acesse o diretório do projeto:
    ```bash
    cd Bomberman-Game
    ```
3.  Instale as dependências do Haskell e prepare o ambiente:
    ```bash
    stack setup
    ```

## Uso

Execute o comando a seguir para iniciar o jogo:
```bash
stack run
```
*(Nota: Se o comando acima não funcionar, pode precisar especificar o ficheiro principal: `stack run app/Main.hs`)*

## Como jogar

1.  Ao executar a aplicação, o jogo será iniciado diretamente no seu terminal.
2.  Mova o personagem por meio das teclas `W` (Cima), `A` (Esquerda), `S` (Baixo) e `D` (Direita).
3.  Utilize a tecla `Espaço` para plantar bombas e destruir as caixas. Você pode ter no máximo 3 bombas simultâneas no mapa.
4.  Encontre a Chave escondida sob uma das caixas para poder usar a porta de saída.
5.  Vença o jogo antes que o seu tempo de 120 segundos acabe.


## Contribuição

Contribuições são bem-vindas! Se deseja melhorar ou adicionar recursos ao Bomberman, siga estas etapas:

1.  Fork o repositório.
2.  Crie um branch para a sua nova funcionalidade (`git checkout -b feature/nova-funcionalidade`).
3.  Faça as alterações desejadas no código.
4.  Commit as suas alterações (`git commit -am 'Adiciona nova funcionalidade'`).
5.  Push para o branch (`git push origin feature/nova-funcionalidade`).
6.  Abra um Pull Request.
