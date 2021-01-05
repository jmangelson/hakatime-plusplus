BEGIN;
    CREATE EXTENSION btree_gist;

    CREATE TABLE IF NOT EXISTS users (
        username TEXT PRIMARY KEY,
        hashed_password BYTEA NOT NULL,
        salt_used BYTEA NOT NULL
    );

    CREATE TABLE IF NOT EXISTS auth_tokens (
        token TEXT PRIMARY KEY,
        owner TEXT REFERENCES users(username) NOT NULL,
        token_expiry TIMESTAMP,
        last_usage TIMESTAMP
    );

    CREATE TABLE IF NOT EXISTS refresh_tokens (
        refresh_token TEXT PRIMARY KEY,
        owner TEXT REFERENCES users(username) NOT NULL,
        token_expiry TIMESTAMP
    );

    CREATE TABLE IF NOT EXISTS projects (
        name TEXT PRIMARY KEY,
        description TEXT,
        owner TEXT REFERENCES users(username) NOT NULL,
        dependencies TEXT [],
        repository TEXT
    );

    CREATE TABLE IF NOT EXISTS heartbeats (
        id SERIAL PRIMARY KEY,
        editor TEXT,
        plugin TEXT,
        platform TEXT,
        machine TEXT,
        sender TEXT REFERENCES users(username) NOT NULL,
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
        owner TEXT REFERENCES users(username) NOT NULL,
        UNIQUE (name, owner)        
    );

    CREATE TABLE IF NOT EXISTS sub_categories (
        id SERIAL PRIMARY KEY,
        name TEXT,
        major_category_id INT REFERENCES major_categories(id) NOT NULL,
        owner TEXT REFERENCES users(username) NOT NULL,
        UNIQUE (name, owner, major_category_id)        
    );

    CREATE TABLE IF NOT EXISTS sub_sub_categories (
        id SERIAL PRIMARY KEY,
        name TEXT,
        major_category_id INT REFERENCES major_categories(id) NOT NULL,
        sub_category_id INT REFERENCES sub_categories(id) NOT NULL,
        owner TEXT REFERENCES users(username) NOT NULL,
        UNIQUE (name, owner, major_category_id, sub_category_id)        
    );

    CREATE TABLE IF NOT EXISTS task_states (
        id SERIAL PRIMARY KEY,    
        name TEXT,
        owner TEXT REFERENCES users(username) NOT NULL,
        UNIQUE (name, owner)        
    );

    CREATE TABLE IF NOT EXISTS classes (
        id SERIAL PRIMARY KEY,
        name TEXT,
        owner TEXT REFERENCES users(username) NOT NULL,
        UNIQUE (name, owner)
    );

    CREATE TABLE IF NOT EXISTS project_lists (
        id SERIAL PRIMARY KEY,
        name TEXT,
        owner TEXT REFERENCES users(username) NOT NULL,
        UNIQUE (name, owner)
    );

    CREATE TABLE IF NOT EXISTS task_lists (
        id SERIAL PRIMARY KEY,
        name TEXT,
        owner TEXT REFERENCES users(username) NOT NULL,
        UNIQUE (name, owner)
    );

    CREATE TABLE IF NOT EXISTS contexts (
        id SERIAL PRIMARY KEY,
        name TEXT,
        owner TEXT REFERENCES users(username) NOT NULL,
        UNIQUE (name, owner)
    );  

    CREATE TABLE IF NOT EXISTS gtd_projects (
        id SERIAL PRIMARY KEY,
        name TEXT,
        sub_sub_category_id INT REFERENCES sub_sub_categories(id) NOT NULL,
        list INT REFERENCES project_lists(id) NOT NULL,
        status INT REFERENCES task_states(id),
        class INT REFERENCES classes(id),
        time_created TIMESTAMP NOT NULL,
        deadline TIMESTAMP,
        owner TEXT REFERENCES users(username) NOT NULL,
        UNIQUE (name, owner, sub_sub_category_id)        
    );

    CREATE TABLE IF NOT EXISTS tasks (
        id SERIAL PRIMARY KEY,
        name TEXT,
        sub_sub_category_id INT REFERENCES sub_sub_categories(id) NOT NULL,
        gtd_project_id INT REFERENCES gtd_projects(id),
        list INT REFERENCES task_lists(id) NOT NULL,
        context INT REFERENCES contexts(id),
        status INT REFERENCES task_states(id) NOT NULL,
        class INT REFERENCES classes(id),
        time_created TIMESTAMP NOT NULL,
        deadline TIMESTAMP,
        owner TEXT REFERENCES users(username) NOT NULL,
        UNIQUE (name, owner, sub_sub_category_id)
    );

    CREATE TABLE IF NOT EXISTS time_log (
        id SERIAL PRIMARY KEY,
        sub_sub_category_id INT REFERENCES sub_sub_categories(id) NOT NULL,
        task_id INT REFERENCES tasks(id),
        gtd_project_id INT REFERENCES gtd_projects(id),
        start_time TIMESTAMP NOT NULL,
        end_time TIMESTAMP,
        owner TEXT REFERENCES users(username) NOT NULL,
        CONSTRAINT overlapping_times EXCLUDE USING GIST (
            owner WITH =,
            TSRANGE(start_time, end_time) WITH &&
        )
    );

    CREATE TABLE IF NOT EXISTS goal_classes (
        id SERIAL PRIMARY KEY,    
        name TEXT,
        owner TEXT REFERENCES users(username) NOT NULL,
        UNIQUE (name, owner)
    );

    CREATE TABLE IF NOT EXISTS goals (
        id SERIAL PRIMARY KEY,
        name TEXT,
        goal_type TEXT, 
        time_created TIMESTAMP NOT NULL,
        start_time TIMESTAMP NOT NULL,
        end_time TIMESTAMP NOT NULL,
        class INT REFERENCES goal_classes(id),
        owner TEXT REFERENCES users(username) NOT NULL,
        UNIQUE (name, owner)
    );


COMMIT;
