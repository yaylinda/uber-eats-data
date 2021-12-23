import datetime
import pytz
from csv_helper import read_csv
from csv_helper import write_csv


def get_restaurant_id_name_map(restaurants):
    """
    """

    map = {}
    
    for restaurant in restaurants:
        restaurant_name = restaurant['Restaurant Name']
        if 'McDonald\'s' in restaurant_name:
            restaurant_name = 'McDonald\'s'
        elif 'The Halal Guys' in restaurant_name:
            restaurant_name = 'The Halal Guys'
        elif 'Chick-fil-A' in restaurant_name:
            restaurant_name = 'Chick-fil-A'
        elif 'Panera' in restaurant_name:
            restaurant_name = 'Panera'
        elif 'KFC' in restaurant_name:
            restaurant_name = 'KFC'
        elif 'Firehouse Subs' in restaurant_name:
            restaurant_name = 'Firehouse Subs'
        elif 'Taco Bell' in restaurant_name:
            restaurant_name = 'Taco Bell'
        elif 'Jack in the Box' in restaurant_name:
            restaurant_name = 'Jack in the Box'
        elif 'North Italia' in restaurant_name:
            restaurant_name = 'North Italia'

        map[restaurant['Restaurant ID']] = restaurant_name

    return map


def get_orders_by_date_by_restaurant(orders, restaurant_id_name_map, central_tz):
    """
    """

    orders_by_date = {}

    for order in orders:
        order_time_str = order['Order Time']
        order_datetime = datetime.datetime.strptime(order_time_str, '%Y-%m-%d %H:%M:%S %z %Z')
        order_datetime_central = order_datetime.astimezone(central_tz)
        order_date_str = order_datetime_central.strftime('%Y-%m-%d')

        if order_date_str not in orders_by_date:
            orders_by_date[order_date_str] = {}
        
        restaurant_name = restaurant_id_name_map[order['Restaurant ID']]

        if restaurant_name not in orders_by_date[order_date_str]:
            orders_by_date[order_date_str][restaurant_name] = {}

        orders_by_date[order_date_str][restaurant_name] = order['Order Price']

    return orders_by_date


def format_data(orders_by_date_by_restaurant, start_year, end_year, unique_restaurant_names):
    """
    """

    data = []

    start_date = datetime.date(start_year, 1, 1)

    while start_date.year < end_year:

        datum = {}
        key = start_date.strftime('%Y-%m-%d')
        datum['date'] = key
        
        if key not in orders_by_date_by_restaurant:
            restaurants_for_day = {}
        else:
            restaurants_for_day = orders_by_date_by_restaurant[key]

        for restaurant in unique_restaurant_names:
            if restaurant in restaurants_for_day:
                datum[restaurant] = restaurants_for_day[restaurant]
            else:
                datum[restaurant] = 0

        data.append(datum)

        start_date = start_date + datetime.timedelta(days = 1)

    return data


def main():
    """
    """

    orders = read_csv(
        'data/Eats/eats_order_details.csv', 
        [
            'Territory',
            'Restaurant ID',
            'Order ID',
            'Order Time',
            'Order Status',
            'Item Name',
            'Customizations',
            'Special Instructions',
            'Item Price',
            'Order Price',
            'Currency'
        ]
    )

    restaurants = read_csv(
        'data/Eats/eats_restaurant_names.csv', 
        [
            'City',
            'Restaurant ID',
            'Restaurant Name',
            'Order Time'
        ]
    )

    restaurant_id_name_map = get_restaurant_id_name_map(restaurants)

    orders_by_date_by_restaurant = get_orders_by_date_by_restaurant(
        orders, 
        restaurant_id_name_map, 
        pytz.timezone("US/Central")
    )

    data = format_data(
        orders_by_date_by_restaurant,
        2020,
        2022, 
        set(restaurant_id_name_map.values())
    )

    write_csv(
        data, 
        'processed_data', 
        ['date'] + list(set(restaurant_id_name_map.values()))
    )


if __name__ == '__main__':
    main()