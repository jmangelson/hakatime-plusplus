BEGIN;
    CREATE TABLE IF NOT EXISTS users (
        username TEXT PRIMARY KEY,
        hashed_password BYTEA NOT NULL,
        salt_used BYTEA NOT NULL
    );

    CREATE TABLE IF NOT EXISTS auth_tokens (
        token TEXT PRIMARY KEY,
        owner TEXT REFERENCES users(username),
        token_expiry TIMESTAMP,
        last_usage TIMESTAMP
    );

    CREATE TABLE IF NOT EXISTS refresh_tokens (
        refresh_token TEXT PRIMARY KEY,
        owner TEXT REFERENCES users(username),
        token_expiry TIMESTAMP
    );

    CREATE TABLE IF NOT EXISTS projects (
        name TEXT PRIMARY KEY,
        description TEXT,
        owner TEXT REFERENCES users(username),
        dependencies TEXT [],
        repository TEXT
    );

    CREATE TABLE IF NOT EXISTS heartbeats (
        id SERIAL PRIMARY KEY,
        editor TEXT,
        plugin TEXT,
        platform TEXT,
        machine TEXT,
        sender TEXT REFERENCES users(username),
        user_agent TEXT,
        branch TEXT,
        category TEXT,
        cursorpos TEXT,
        dependencies TEXT [],
        entity TEXT NOT NULL,
        is_write BOOL,
        language TEXT,
        lineno INT,
        file_lines INT,
        project TEXT REFERENCES projects(name),
        ty TEXT NOT NULL,
        time_sent TIMESTAMP NOT NULL
    );

    CREATE TABLE IF NOT EXISTS major_categories (
        id SERIAL PRIMARY KEY,    
        name TEXT,
        owner TEXT REFERENCES users(username)
    );

    CREATE TABLE IF NOT EXISTS sub_categories (
        id SERIAL PRIMARY KEY,
        name TEXT,
        major_category_id INT REFERENCES major_categories(id)
    );

    CREATE TABLE IF NOT EXISTS sub_sub_categories (
        id SERIAL PRIMARY KEY,
        name TEXT,
        major_category_id INT REFERENCES major_categories(id),
        sub_category_id INT REFERENCES sub_categories(id)                
    );

    CREATE TABLE IF NOT EXISTS task_states (
        name TEXT PRIMARY KEY
    );

    CREATE TABLE IF NOT EXISTS classes (
        name TEXT PRIMARY KEY
    );

    CREATE TABLE IF NOT EXISTS tasks (
        id SERIAL PRIMARY KEY,
        name TEXT,
        major_category_id INT REFERENCES major_categories(id),
        sub_category_id INT REFERENCES sub_categories(id),
        sub_sub_category_id INT REFERENCES sub_sub_categories(id),
        status TEXT REFERENCES task_states(name),
        class TEXT REFERENCES classes(name),
        time_created TIMESTAMP NOT NULL,
        deadline TIMESTAMP NOT NULL
    );

    CREATE TABLE IF NOT EXISTS goal_classes (
        id SERIAL PRIMARY KEY,    
        name TEXT 
    );

    CREATE TABLE IF NOT EXISTS goals (
        id SERIAL PRIMARY KEY,
        name TEXT,
        goal_type TEXT, 
        time_created TIMESTAMP NOT NULL,
        start_time TIMESTAMP NOT NULL,
        end_time TIMESTAMP NOT NULL
    );


COMMIT;
