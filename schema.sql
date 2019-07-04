DROP TABLE IF EXISTS users;
DROP TABLE IF EXISTS skills;
DROP TABLE IF EXISTS users_skills;

CREATE TABLE IF NOT EXISTS "users"("id" uuid  NOT NULL, "first_name" TEXT NOT NULL, "last_name" TEXT NOT NULL, "email" TEXT NOT NULL UNIQUE, "password_hash" BYTEA NOT NULL, "password_salt" BYTEA NOT NULL, "password_out_len" INT8 NOT NULL, "password_iterations" INT8 NOT NULL, "password_memory" INT8 NOT NULL, "password_parallelism" INT8 NOT NULL, UNIQUE("email"), PRIMARY KEY("id"));
CREATE TABLE IF NOT EXISTS "skills"("name" TEXT  NOT NULL, "description" TEXT NOT NULL, "parent_skill" TEXT NULL, PRIMARY KEY("name"), CONSTRAINT "fk0_parent_skill" FOREIGN KEY ("parent_skill") REFERENCES "skills"("name"));
CREATE TABLE IF NOT EXISTS "users_skills"("user_id" uuid NOT NULL, "skill_name" TEXT NOT NULL, "proficiency" INT8 NOT NULL, "hours_spent" INT8 NOT NULL, "desired_work" BOOLEAN NOT NULL, PRIMARY KEY("user_id", "skill_name"), CONSTRAINT "fk0_user_id" FOREIGN KEY ("user_id") REFERENCES "users"("id"), CONSTRAINT "fk1_skill_name" FOREIGN KEY ("skill_name") REFERENCES "skills"("name"));
