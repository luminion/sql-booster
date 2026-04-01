package io.github.luminion.sqlbooster.util;

import io.github.luminion.sqlbooster.model.SqlContext;
import org.junit.Assert;
import org.junit.Test;

import java.util.Map;

public class BeanPropertyUtilsTest {

    @Test
    public void shouldConvertSqlContextToMapWithoutNullMembers() {
        SqlContext<String> sqlContext = new SqlContext<String>();

        Map<String, Object> map = BeanPropertyUtils.toMap(sqlContext);

        Assert.assertTrue(map.containsKey("conditions"));
        Assert.assertTrue(map.containsKey("sorts"));
        Assert.assertTrue(map.containsKey("params"));
        Assert.assertTrue(map.containsKey("and"));
        Assert.assertFalse(map.containsKey("next"));
        Assert.assertFalse(map.containsKey("class"));
    }

    @Test
    public void shouldExposeReadablePropertiesAndIgnoreNulls() {
        SampleBean bean = new SampleBean();
        bean.setName("tester");

        Map<String, Object> map = BeanPropertyUtils.toMap(bean);

        Assert.assertEquals("tester", map.get("name"));
        Assert.assertFalse(map.containsKey("age"));
    }

    public static final class SampleBean {
        private String name;
        private Integer age;

        public String getName() {
            return name;
        }

        public void setName(String name) {
            this.name = name;
        }

        public Integer getAge() {
            return age;
        }

        public void setAge(Integer age) {
            this.age = age;
        }
    }
}
