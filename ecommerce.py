import streamlit as st
import pandas as pd
import plotly.express as px
import os
import json
import requests

# Configuração do título do app
st.set_page_config(page_title="Dashboard de E-commerce", layout="wide")

# Função para carregar os datasets
@st.cache_data
def load_data(folder_path):
    csv_files = {
        "customers": "olist_customers_dataset.csv",
        "geolocation": "olist_geolocation_dataset.csv",
        "orders": "olist_orders_dataset.csv",
        "order_items": "olist_order_items_dataset.csv",
        "order_payments": "olist_order_payments_dataset.csv",
        "order_reviews": "olist_order_reviews_dataset.csv",
        "products": "olist_products_dataset.csv",
        "sellers": "olist_sellers_dataset.csv",
        "categories": "product_category_name_translation.csv",
    }
    dataframes = {}
    for key, file in csv_files.items():
        file_path = os.path.join(folder_path, file)
        dataframes[key] = pd.read_csv(file_path)
    return dataframes


# Diretório dos arquivos CSV
folder_path = r"/workspaces/ecommerce-dashboard/extracted_files"

data = load_data(folder_path)

# Carregando os datasets
customers = data["customers"]
geolocation = data["geolocation"]
orders = data["orders"]
order_items = data["order_items"]
order_payments = data["order_payments"]
order_reviews = data["order_reviews"]
products = data["products"]
sellers = data["sellers"]
categories = data["categories"]

orders['order_delivered_customer_date'] = pd.to_datetime(orders['order_delivered_customer_date'])


merged_geo_customers = customers.merge(
    geolocation, 
    left_on="customer_zip_code_prefix", 
    right_on="geolocation_zip_code_prefix",
    how="left"  # use left join para manter todos os clientes
) 

geoloc_orders = orders.merge(
    merged_geo_customers, 
    on="customer_id", 
    how="left"
)

order_payments = order_payments[order_payments["payment_type"] != "not_defined"]

# _________________________________ Traduções _________________________________
payment_translation = {
    "credit_card": "Cartão de Crédito",
    "debit_card": "Cartão de Débito",
    "voucher": "Brinde",
    "boleto": "Boleto",
    "paypal": "PayPal",
    "bank_transfer": "Transferência Bancária",
}

order_payments["payment_type"] = order_payments["payment_type"].replace(payment_translation)

orders = orders[orders["order_status"] != "unavailable"]
order_status_translation = {
    "delivered": "Entregue",
    "shipped": "Em Trânsito",
    "canceled": "Devolução",
    "invoiced": "Faturado",
    "processing": "Preparando",
    "created": "Aguardando Pagamento",
    "approved": "Pagamento Aprovado",
}
orders["order_status"] = orders["order_status"].replace(order_status_translation)


category_translation = {
    "bed_bath_table": "Cama-Banho-Mesa",
    "health_beauty": "Saúde e Beleza",
    "sports_leisure": "Lazer Esportivo",
    "furniture_decor": "Móveis",
    "computers_accessories": "Informática",
    "housewares": "Utilidades domésticas",
    "watches-gifts": "Relógios",
    "telephony": "Celulares",
    "garden_tools": "Jardinagem",
    "auto": "Automóvel",
}
product_sales = order_items.merge(products, on="product_id").merge(categories, on="product_category_name", how="left")
product_sales = product_sales[product_sales["product_category_name"].isin(category_translation.keys())]
product_sales["product_category_name_english"] = product_sales["product_category_name"].replace(category_translation)

# _________________________________ Traduções _________________________________

# Títulos do dashboard
st.title("Dashboard de E-commerce 📊")
st.markdown("Análise dos dados da operação de um e-commerce brasileiro.")

# Dividindo em colunas para exibir KPIs
col1, col2, col3 = st.columns(3)

# Indicadores principais
col1.metric("Total de Clientes", customers["customer_id"].nunique())
col2.metric("Total de Pedidos", orders["order_id"].nunique())
col3.metric("Total de Produtos", products["product_id"].nunique())

# Gráfico 1: Situação dos Pedidos (log)
st.subheader("Status dos Pedidos")
order_status_count = orders["order_status"].value_counts().reset_index()
order_status_count.columns = ["Status", "Quantidade"]
fig1 = px.bar(
    order_status_count,
    x="Status",
    y="Quantidade",
    title="Pedidos x Status (log)",
    text="Quantidade",
    log_y=True  # Adiciona escala logarítmica no eixo Y
)
st.plotly_chart(fig1, use_container_width=True)

# Gráfico 2: Avaliações dos Pedidos
st.subheader("Distribuição de Avaliações")
review_score_count = order_reviews["review_score"].value_counts().reset_index()
review_score_count.columns = ["Nota", "Quantidade"]
fig4 = px.bar(review_score_count, x="Nota", y="Quantidade", title="Distribuição de Avaliações dos Pedidos", text="Quantidade")
st.plotly_chart(fig4, use_container_width=True)


# Gráfico 3: Produtos mais Vendidos por Categoria
st.subheader("Vendas x Categoria")
product_sales = order_items.merge(products, on="product_id").merge(categories, on="product_category_name", how="left")
product_sales["product_category_name"] = product_sales["product_category_name"].replace(category_translation)
category_sales = product_sales["product_category_name"].value_counts().reset_index().head(10)
category_sales.columns = ["Categoria", "Quantidade Vendida"]
fig3 = px.bar(
    category_sales,
    x="Categoria",
    y="Quantidade Vendida",
    title="Top 10 Categorias de Produtos mais Vendidos",
    text="Quantidade Vendida"
)
st.plotly_chart(fig3, use_container_width=True)


# Gráfico 4 : Estados
# Filtro: Análise por Estado
st.subheader("Análise de Clientes por Estado")
customer_state_count = customers["customer_state"].value_counts().reset_index()
customer_state_count.columns = ["Estado", "Quantidade"]
fig5 = px.bar(customer_state_count, x="Estado", y="Quantidade", title="Clientes por Estado", text="Quantidade")
st.plotly_chart(fig5, use_container_width=True)

# gráfico 5 - mapa

geojson_url = "https://raw.githubusercontent.com/codeforamerica/click_that_hood/master/public/data/brazil-states.geojson"
geojson_data = requests.get(geojson_url).json()

# Dados de exemplo: Agrupar pedidos por estado
state_order_counts = geoloc_orders["customer_state"].value_counts().reset_index()
state_order_counts.columns = ["Estado", "Quantidade"]

# Criar o mapa usando o GeoJSON
fig7 = px.choropleth(
    state_order_counts,
    geojson=geojson_data,  # GeoJSON com a geometria dos estados brasileiros
    locations="Estado",  # Coluna com as siglas dos estados
    featureidkey="properties.sigla",  # Chave do GeoJSON que corresponde às siglas
    color="Quantidade",  # Coluna usada para colorir o mapa
    title="Pedidos por Estado (Brasil)",
    color_continuous_scale="Viridis",  # Escala de cores
)

# Ajustar o foco do mapa para o Brasil
fig7.update_geos(
    fitbounds="locations",  # Ajusta o zoom para os estados exibidos
    visible=False  # Oculta os limites padrão do globo
)

# Exibir o mapa
st.plotly_chart(fig7, use_container_width=True)
# gráfico 5 - mapa (limite)

# Gráfico 6: Receita por Tipo de Pagamento (R$ Ordenado maior para o menor)
st.subheader("Receita por Tipo de Pagamento")
revenue_payment_type = order_payments.groupby("payment_type")["payment_value"].sum().reset_index()
revenue_payment_type.columns = ["Tipo de Pagamento", "Receita Total"]
revenue_payment_type = revenue_payment_type.sort_values(by="Receita Total", ascending=False)
revenue_payment_type["Receita Formatada"] = revenue_payment_type["Receita Total"].apply(lambda x: f"R$ {x:,.2f}".replace(",", ".").replace(".", ",", 1))

# "Receita Total"
fig6 = px.bar(
    revenue_payment_type,
    x="Tipo de Pagamento",
    y="Receita Total",
    title="Receita por Tipo de Pagamento (R$)",
    text="Receita Formatada"
)
st.plotly_chart(fig6, use_container_width=True)

# Gráfico 7: Distribuição de Pagamentos
st.subheader("Métodos de Pagamento")
payment_type_count = order_payments["payment_type"].value_counts().reset_index()
payment_type_count.columns = ["Tipo de Pagamento", "Quantidade"]
fig2 = px.pie(payment_type_count, values="Quantidade", names="Tipo de Pagamento", title="Distribuição de Tipos de Pagamento")
st.plotly_chart(fig2, use_container_width=True)



# Rodapé
st.markdown("---")
st.caption("Criado por Matheus Aragão Cavalcante")


#Comando pra abrir pelo terminal: python -m streamlit run ecommerce.py
