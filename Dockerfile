FROM php:8-apache
COPY www/ /var/www/html/ecce
RUN echo "RedirectMatch ^/$ /ecce/" > /var/www/html/.htaccess
