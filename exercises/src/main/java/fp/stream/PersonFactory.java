package fp.stream;

/**
 * Created by apple on 9/24/16.
 */
public interface PersonFactory<P extends Person> {
    P create(String name, int age);
}


class PersonFactoryTest{
    public static void main(String[] args){
        PersonFactory personFactory = Person::new;

        Person person = personFactory.create("peter",32);

        System.out.print(person.toString());
    }
}