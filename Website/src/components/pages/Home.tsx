import { Container } from "@nextui-org/react";
import styled from '@emotion/styled';
import Banner from "../molecules/Banner";

const Content = styled.div`
  min-height: max-content;
`;

export default () => (
  <Container>
    <Content>
      <Banner />
    </Content>
  </Container>
);
