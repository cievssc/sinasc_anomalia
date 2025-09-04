# Painel Sinasc / Anomalia Congênita

![R](https://img.shields.io/badge/R-276DC3?style=for-the-badge&logo=r&logoColor=white)
![Shiny](https://img.shields.io/badge/Shiny-1FA8E0?style=for-the-badge&logo=r&logoColor=white)
![SUS](https://img.shields.io/badge/SUS-blue?style=for-the-badge)
![License](https://img.shields.io/badge/License-MIT-yellow.svg?style=for-the-badge)

Painel dos dados elaborado para explorar e sintetizar os dados do Sistema de Informações sobre Nascidos Vivos (SINASC), agregando informações obtidas
no Cadastro Nacional de Estabelecimentos de Saúde (CNES) e 

## 🌟 Visão geral da aplicação

<img src="./www/images/sinasc_app.gif" width="60%"  />


## 📁 Estrutura da aplicação

```
sinasc_app/
│
├── server/                 # Server para cada aba da aplicação
│   ├── server_anomalia.R
│   ├── server_hospital.R
│   └── server_nascido.R
├── ui/                     # User interface para cada aba da aplicação
│   ├── ui_anomalia.R
│   ├── ui_hospital.R
│   └── ui_nascido.R
├── www/                    # Web resources
│   ├── apexchart/          # Scripts com as funções para uso do apexcharts
│   ├── css/                # Custom CSS 
│   ├── images/             # arquivos de imagens
│   ├── JS/                 # Arquivos JS necessários para o uso do apexcharts e do tabler
│   └── tablerdash/         # Scripts com as funções para uso do template do tabler.io
├── global.R                # configurações globais 
├── server.R                # configurações do server
├── ui.R                    # configurações do ui
├── *.RData                 # arquivos com os dados (tabelas sem tratamento, mapas e dicionários de referência)
├── treating_data.R         # script para tratamento dos dados
├── variaveis_ext.R         # script para leitura dos dados
├── README.md
│
└── getting_data/           # diretório com os scripts para obter os dados das fontes
```
## :iphone: Obtenção e Atualização dos dados

Até o momento da publicação da aplicação, 2023 é o último ano dos dados do Sinasc disponibilizado pelo DATASUS. 

Para atualização dos dados, basta adicionar os anos na linha 20 do script _wrapping\_data\_anomaliza\_sinasc.R_ e rodá-lo. Os códigos já estão configurados para
fazer o processo de ETL (_Extract, Transform and Loading_) dos dados, diretamente do ftp do DATASUS. 

## ⚙️ Customização

Esta aplicação foi desenvolvida de modo a poder demonstrar informações de qualquer uma dos estados brasileiros. Para tal, além da modificação dos dados do Sinasc 
(script  _wrapping\_data\_anomaliza\_sinasc.R_), será necessário rodar o script  _wrapping\_estabelecimentos.R_ para atualizar os dados do CNES e
 _wrapping\_regioes.R_ para adequar as tabelas de regiões.

## 📄 License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

---

⭐ Se esse repositório foi útil, retribua nos dando uma estrela!
