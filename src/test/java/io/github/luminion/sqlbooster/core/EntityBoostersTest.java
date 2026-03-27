package io.github.luminion.sqlbooster.core;

import io.github.luminion.sqlbooster.model.SqlContext;
import io.github.luminion.sqlbooster.support.BoosterTestFixtures;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

public class EntityBoostersTest {

    private BoosterTestFixtures.TestBooster booster;

    @Before
    public void setUp() {
        BoosterTestFixtures.registerTestResolver();
        booster = new BoosterTestFixtures.TestBooster();
        BoosterRegistry.registerDefault("testBooster", booster);
    }

    @After
    public void tearDown() {
        BoosterTestFixtures.clearRegistries();
    }

    @Test
    public void entityInterfaceShouldResolveDefaultResultType() {
        Booster<BoosterTestFixtures.TestEntity, BoosterTestFixtures.TestView> resolved = EntityBoosters.booster(
                BoosterTestFixtures.TestEntity.class);

        BoosterTestFixtures.TestView view = resolved.voById(1L);
        assertEquals("tom", view.getName());
    }

    @Test
    public void explicitTargetTypeShouldUseAdapter() {
        Booster<BoosterTestFixtures.TestEntity, BoosterTestFixtures.TestDto> resolved = EntityBoosters.booster(
                BoosterTestFixtures.TestEntity.class, BoosterTestFixtures.TestDto.class);

        BoosterTestFixtures.TestDto dto = resolved.voById(1L);
        assertEquals("tom", dto.getName());
        assertEquals(Integer.valueOf(1), dto.getState());
    }

    @Test
    public void entityLambdaBoosterShouldUseRegisteredDefaultBooster() {
        BoosterTestFixtures.TestEntity entity = new BoosterTestFixtures.TestEntity();
        entity.lambdaBooster().eq(BoosterTestFixtures.TestEntity::getName, "tom").list();

        SqlContext<BoosterTestFixtures.TestEntity> lastContext = booster.getLastContext();
        assertNotNull(lastContext);
        assertEquals(1, lastContext.getConditions().size());
    }

    @Test(expected = IllegalStateException.class)
    public void duplicateDefaultBoosterRegistrationShouldFailFast() {
        BoosterRegistry.registerDefault("anotherTestBooster", new BoosterTestFixtures.TestBooster());
    }
}
