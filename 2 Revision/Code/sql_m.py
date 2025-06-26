# manejo de los formatos sql y su analisis de datos en general
import sqlite3 as sq
import pandas as pd
from collections import Counter

def see_tables(conexion):
    cursor=conexion.cursor()
    query="""SELECT name FROM sqlite_master WHERE type='table';"""
    cursor.execute(query)
    return [ e[0] for e in cursor.fetchall()]
    
def ver_tabla (name, conexion):
    """ funcion para poder llamar a una de las tablas en formato df. necesita el nombre de la tabla entre comillas"""
    sql_query=f"SELECT * FROM {name}"
    df1 = pd.read_sql_query(sql_query, conexion)
    return df1
def see_table_count(name, conexion, count):
    #Funcion para visualizar una cantidad count de registros de la tabla
    sql_query=f'SELECT * FROM {name} LIMIT {count}'
    df1=pd.read_sql_query(sql_query, conexion)
    return df1
def add_registers_to_table(name, df, conexion, if_exists='append'):
    try:
        # Obtener las columnas de la tabla actual
        cursor = conexion.cursor()
        cursor.execute(f"PRAGMA table_info({name})")
        columns_in_table = [info[1] for info in cursor.fetchall()]

        # Filtrar el DataFrame para que solo contenga las columnas que existen en la tabla
        df_filtered = df[[col for col in df.columns if col in columns_in_table]]
        df_filtered.to_sql(name, conexion, if_exists=if_exists, index=False)
        print(f'Se han agregado {len(df)} registros a la tabla {name}')
    except Exception as e:
        print(f'Error al agregar los registros a la tabla {name}')

def get_last_non_null_values(table_name, conexion):
    """
    Obtiene los últimos valores no nulos y no vacíos de cada columna de una tabla en una base de datos.
    
    Parámetros:
    - table_name: Nombre de la tabla en la base de datos.
    - conexion: Objeto de conexión a la base de datos.
    
    Retorna:
    - Un diccionario con el último valor no nulo de cada columna.
    """
    # Primero, obtenemos los nombres de las columnas de la tabla
    columns_query = f"PRAGMA table_info({table_name});"
    columns_df = pd.read_sql_query(columns_query, conexion)
    columns = columns_df['name'].tolist()

    # Diccionario para almacenar los últimos valores
    last_values = {}

    # Consulta SQL para obtener el último valor no nulo por columna
    for col in columns:
        query = f"""
        SELECT {col}
        FROM {table_name}
        WHERE {col} IS NOT NULL AND {col} != '' AND {col} != 'NA'
        ORDER BY rowid DESC
        LIMIT 1;
        """
        result = pd.read_sql_query(query, conexion)

        if not result.empty:
            last_values[col] = result[col].iloc[0]
        else:
            last_values[col] = None  # Si no hay valores válidos, se asigna None

    return last_values
def dict_from_table(table_name, conexion, col1="trail", col2="id"):
    """
    Este módulo toma el nombre de una tabla y una conexión a la base de datos, y devuelve un diccionario
    con los valores de las columnas especificadas.
    
    table_name: Nombre de la tabla de la base de datos.
    conexion: Conexión activa a la base de datos SQLite.
    col1: Nombre de la primera columna para las claves del diccionario.
    col2: Nombre de la segunda columna para los valores del diccionario.
    """
    # Crear un cursor para la conexión
    cursor = conexion.cursor()

    # Ejecutar una consulta para obtener las dos columnas especificadas
    query = f"SELECT {col1}, {col2} FROM {table_name}"
    cursor.execute(query)

    # Obtener todos los resultados de la consulta
    results = cursor.fetchall()

    # Crear un diccionario a partir de los resultados
    dict_result = {row[0]: row[1] for row in results}

    return dict_result
def get_last_from_col(table_name, col_name, conexion):
    cursor=conexion.cursor()
    # Consulta para obtener el último valor de la columna
    query = f"SELECT {col_name} FROM {table_name} ORDER BY ROWID DESC LIMIT 1"
    cursor.execute(query)
    result=cursor.fetchone()
    if result:
        return result[0]
    else: return None

    

        
    