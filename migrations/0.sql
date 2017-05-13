CREATE TABLE rsvps (
    id serial PRIMARY KEY,
    k text NOT NULL,
    lodging text,
    friday boolean,
    saturday boolean,
    food text,
    confirmed_at timestamptz
);

CREATE TABLE people (
    id serial PRIMARY KEY,
    name text NOT NULL,
    locked boolean NOT NULL,
    rsvp_id integer NOT NULL REFERENCES rsvps(id),
    include boolean NOT NULL DEFAULT true
);

CREATE UNIQUE INDEX rsvps_k_idx ON rsvps (k);
