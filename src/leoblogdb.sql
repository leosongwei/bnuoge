-- ARTICLE --
CREATE TABLE article (
	aid serial PRIMARY KEY,
	title varchar NOT NULL,
	body varchar NOT NULL,
	tags varchar,
	date timestamp without time zone NOT NULL,
	url varchar NOT NULL UNIQUE
);

-- REPLY --
CREATE TABLE reply (
	rid serial PRIMARY KEY,
	aid int references article(aid),
	name varchar NOT NULL,
	email varchar NOT NULL,
	website varchar,
	date timestamp without time zone NOT NULL,
	body varchar
);
