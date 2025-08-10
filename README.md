# Bomberman
Este repositório é destinado a criar um simples jogo inspirado no
bomberman utilizando programação funcional!
## Execução ##
### Pré Requisitos:
Para executar o código em máquinas windows é recomendável utilizar o subsistema do Windows para Linux(WSL), estando [aqui](https://learn.microsoft.com/pt-br/windows/wsl/install) o manual de instalação.
É necessário também instalar o Haskell, recomendamos utilizar o GHCup para isso, disponível [aqui](https://www.haskell.org/ghcup/install/). Tendo instalado o Haskell, instale o Stack por meio do comando `curl -sSL https://get.haskellstack.org/`
### Executando o Stack: 
Execute o código por meio do comando `stack run app/Main.hs`
## Jogabilidade 🎮
Mova o personagem por meio das teclas `W`(Cima), `A`(Esquerda), `S`(Baixo), `D`(Direita) e utilize a tecla `Espaço` para plantar bombas. Você possui um limite de 3 bombas simultâneas presentes no mapa, e pode morrer caso seja atingido por uma explosão. 
O mapa possui uma porta a qual você pode usar para vencer, mas para poder passar pela porta, é preciso encontrar uma Chave no meio das paredes destrutivas, tudo isso antes que seu tempo de 120 segundos acabe. **Boa Sorte**
