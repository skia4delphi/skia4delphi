import { Container, Text } from "@nextui-org/react";
import styled from '@emotion/styled';

const Content = styled.div`
  height: calc(100vh - 130px);
  min-height: max-content;
  display: flex;
  justify-content: center;
  flex-direction: column;
`;

export default () => (
  <Container>
    <Content>
      <Text h2>
        About Skia4Delphi
      </Text>
      <Text>
        The project was founded by Vinicius Felipe Botelho Barbosa, technology enthusiast, Systems Engineering student, with over 15 years of experience with Delphi development, with co-participation of Paulo César Botelho Barbosa.
        The library was made from developer to developer. Hope it helps and somehow adds something for everyone.
        Enjoy!
      </Text>
      <hr />
      <Text h2>
        Contributions
      </Text>
      <Text>
        Skia4Delphi is an open source library that requires a lot of personal investment,
        to do it with the highest quality we have received support
        of many different people. We would like to be very grateful to <b>Jim McKeeth</b> and <b>Ian Barker</b>
        for giving us support and guidance on this journey.
        <br />
        Nevertheless, we invite you developer, interested in helping us to build a great tool
        to collaborate and share some of your time. We are a very friendly and open-minded community.
        and we hope we can do great things together.
      </Text>
      <hr />
      <Text h2>
        What is Skia?
      </Text>
      <Text>
        Skia is an open source library for drawing 2D Text, Geometries, Images, focused on accurate, high quality and high performance rendering, which provides common APIs that work across a variety of hardware and software platforms, used by Chrome, Android, Flutter, Xamarin, Mozilla Firefox, Firefox OS, and many other products.
      </Text>
    </Content>
  </Container>
);
