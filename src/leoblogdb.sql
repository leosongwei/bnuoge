-- ARTICLE --
CREATE TABLE article (
	aid serial PRIMARY KEY,
	title varchar(255) NOT NULL,
	body varchar NOT NULL,
	tags varchar(255),
	date timestamp without time zone NOT NULL,
	url varchar(63) NOT NULL UNIQUE,
	click int DEFAULT 0
);

-- REPLY --
CREATE TABLE reply (
	rid serial PRIMARY KEY,
	aid int references article(aid),
	name varchar(20) NOT NULL,
	email varchar(255) NOT NULL,
	website varchar(255),
	date timestamp without time zone NOT NULL,
	body varchar(4000)
);

-- PAGE --
CREATE TABLE page (
	pid serial PRIMARY KEY,
	title varchar(255) NOT NULL,
	body varchar NOT NULL,
	tags varchar(255),
	date timestamp without time zone NOT NULL,
	url varchar(63) NOT NULL UNIQUE,
	click int DEFAULT 0
);

-- IMAGE --
CREATE TABLE image (
	iid serial PRIMARY KEY,
	name varchar(255) NOT NULL,
	time timestamp NOT NULL,
	describe varchar,
	body bytea
);

-- LOG --
CREATE TABLE log (
	lid serial PRIMARY KEY,
	cookie varchar(255),
	ip varchar(255) NOT NULL,
	time timestamp NOT NULL,
	useragent varchar(255),
	request varchar NOT NULL,
	adminp boolean NOT NULL
);
