package io.github.luminion.sqlbooster.extension.mybatis;

import io.github.luminion.sqlbooster.metadata.TableMeta;
import org.junit.Assert;
import org.junit.Test;

public class DefaultTableResolverTest {

    @Test
    public void shouldResolveUnderscoreNamesAndIdProperty() {
        DefaultTableResolver resolver = new DefaultTableResolver(true, 100);

        TableMeta tableMeta = resolver.resolve(SampleUser.class);

        Assert.assertEquals("sample_user", tableMeta.getTableName());
        Assert.assertEquals("id", tableMeta.getIdPropertyName());
        Assert.assertEquals("a.user_name", tableMeta.getPropertyToColumnAliasMap().get("userName"));
        Assert.assertEquals("a.created_at", tableMeta.getPropertyToColumnAliasMap().get("createdAt"));
    }

    @Test
    public void shouldKeepOriginalPropertyNamesWhenUnderscoreDisabled() {
        DefaultTableResolver resolver = new DefaultTableResolver(false, 200);

        TableMeta tableMeta = resolver.resolve(NoIdSample.class);

        Assert.assertEquals("no_id_sample", tableMeta.getTableName());
        Assert.assertNull(tableMeta.getIdPropertyName());
        Assert.assertEquals("a.userName", tableMeta.getPropertyToColumnAliasMap().get("userName"));
    }

    public static final class SampleUser {
        public Long getId() {
            return 1L;
        }

        public String getUserName() {
            return "tester";
        }

        public String getCreatedAt() {
            return "2025-01-01";
        }
    }

    public static final class NoIdSample {
        public String getUserName() {
            return "tester";
        }
    }
}
