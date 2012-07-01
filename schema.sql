DROP TABLE todo;

CREATE TABLE todo (
       id SERIAL PRIMARY KEY,
       created_on TIMESTAMP not null DEFAULT now(),
       user_id INT references snap_auth_user(uid),
       descr TEXT
);

INSERT INTO todo (user_id, descr) VALUES (1, 'buy pineapple');
INSERT INTO todo (user_id, descr) VALUES (1, 'buy loudspeakers for pc');
INSERT INTO todo (user_id, descr) VALUES (1, 'buy new pants');
