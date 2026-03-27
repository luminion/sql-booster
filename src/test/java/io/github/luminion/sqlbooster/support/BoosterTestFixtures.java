package io.github.luminion.sqlbooster.support;

import io.github.luminion.sqlbooster.core.BoosterEntity;
import io.github.luminion.sqlbooster.core.BoosterService;
import io.github.luminion.sqlbooster.core.BoosterSupport;
import io.github.luminion.sqlbooster.function.SFunc;
import io.github.luminion.sqlbooster.metadata.BoosterRegistry;
import io.github.luminion.sqlbooster.metadata.TableMetaRegistry;
import io.github.luminion.sqlbooster.metadata.TableResolver;
import io.github.luminion.sqlbooster.model.SqlContext;
import io.github.luminion.sqlbooster.util.LambdaUtils;

import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

public abstract class BoosterTestFixtures {

    public static void registerTestResolver() {
        TableMetaRegistry.removeTableResolver(TestTableResolver.class);
        TableMetaRegistry.addTableResolver(new TestTableResolver());
    }

    public static void clearRegistries() {
        TableMetaRegistry.removeTableResolver(TestTableResolver.class);
        BoosterRegistry.clear();
    }

    public static class TestEntity implements BoosterEntity<TestEntity, TestView> {
        private Long id;
        private String name;
        private Integer age;
        private Integer state;
        private Integer role;

        public Long getId() {
            return id;
        }

        public void setId(Long id) {
            this.id = id;
        }

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

        public Integer getState() {
            return state;
        }

        public void setState(Integer state) {
            this.state = state;
        }

        public Integer getRole() {
            return role;
        }

        public void setRole(Integer role) {
            this.role = role;
        }
    }

    public static class TestView {
        private Long id;
        private String name;
        private Integer state;

        public Long getId() {
            return id;
        }

        public void setId(Long id) {
            this.id = id;
        }

        public String getName() {
            return name;
        }

        public void setName(String name) {
            this.name = name;
        }

        public Integer getState() {
            return state;
        }

        public void setState(Integer state) {
            this.state = state;
        }
    }

    public static class TestDto {
        private String name;
        private Integer state;

        public String getName() {
            return name;
        }

        public void setName(String name) {
            this.name = name;
        }

        public Integer getState() {
            return state;
        }

        public void setState(Integer state) {
            this.state = state;
        }
    }

    public static class TestBooster implements BoosterSupport<TestEntity, TestView> {
        private SqlContext<TestEntity> lastContext;

        @Override
        public Class<TestEntity> boosterEntityClass() {
            return TestEntity.class;
        }

        @Override
        public Class<TestView> boosterResultClass() {
            return TestView.class;
        }

        public SqlContext<TestEntity> getLastContext() {
            return lastContext;
        }

        @Override
        public List<TestView> doFetch(SqlContext<TestEntity> sqlContext, Object page) {
            this.lastContext = sqlContext;
            return Collections.singletonList(sampleView());
        }

        private TestView sampleView() {
            TestView view = new TestView();
            view.setId(1L);
            view.setName("tom");
            view.setState(1);
            return view;
        }
    }

    public static class TestService implements BoosterService<TestEntity, TestView> {
    }

    public static class TestTableResolver implements TableResolver {

        @Override
        public int getPriority() {
            return Integer.MIN_VALUE;
        }

        @Override
        public <T> String getTableName(Class<T> entityClass) {
            return entityClass == TestEntity.class ? "test_entity" : null;
        }

        @Override
        public <T> String getIdPropertyName(Class<T> entityClass) {
            return entityClass == TestEntity.class ? "id" : null;
        }

        @Override
        public <T, R> String getGetterPropertyName(SFunc<T, R> getter) {
            Class<T> getterClass = LambdaUtils.resolveGetterClass(getter);
            if (getterClass == TestEntity.class) {
                return LambdaUtils.resolveGetterPropertyName(getter);
            }
            return null;
        }

        @Override
        public <T> Map<String, String> getPropertyToColumnAliasMap(Class<T> entityClass) {
            if (entityClass != TestEntity.class) {
                return Collections.emptyMap();
            }
            Map<String, String> map = new LinkedHashMap<>();
            map.put("id", "id");
            map.put("name", "name");
            map.put("age", "age");
            map.put("state", "state");
            map.put("role", "role");
            return map;
        }
    }
}